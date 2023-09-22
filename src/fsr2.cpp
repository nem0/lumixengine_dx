#include "engine/engine.h"
#include "engine/geometry.h"
#include "engine/log.h"
#include "engine/math.h"
#include "engine/stream.h"
#include "renderer/draw_stream.h"
#include "renderer/pipeline.h"
#include "renderer/renderer.h"
#include "fsr2.h"

#include "ffx_fsr2.h"
#include "dx12/ffx_fsr2_dx12.h"
#pragma comment(lib, "ffx_fsr2_api_dx12_x64d.lib")
#pragma comment(lib, "ffx_fsr2_api_x64d.lib")

#undef near

namespace Lumix::gpu {

void* getDX12CommandList();
void* getDX12Device();
void* getDX12Resource(TextureHandle h);
void resetCommandList();

struct FSR2DispatchParams {
	float time_delta;
	u32 render_width;
	u32 render_height;
	float near_plane;
	float fov;
	float jitter_x;
	float jitter_y;
	TextureHandle color;
	TextureHandle depth;
	TextureHandle motion_vectors;
	TextureHandle output;
};

struct FSR2RendererPlugin : Lumix::RenderPlugin, ISystem {
	FSR2RendererPlugin(Engine& engine)
		: m_allocator(engine.getAllocator())
		, m_contexts(engine.getAllocator())
	{}

	~FSR2RendererPlugin() { ASSERT(m_contexts.empty()); }
	
	struct FSR2Context {
		FSR2Context(IAllocator& allocator) : scratch_buffer(allocator) {}
		~FSR2Context() { ffxFsr2ContextDestroy(&context); }

		Pipeline* pipeline;
		IVec2 size = IVec2(0);
		FfxFsr2Context context;
		OutputMemoryStream scratch_buffer;
	};

	const char* getName() const override { return "fsr2"; }
	i32 getVersion() const override { return 0; }
	void serialize(OutputMemoryStream& serializer) const override {}
	bool deserialize(i32 version, InputMemoryStream& serializer) override { return true; }

	void systemAdded(ISystem& system) override {
		if (equalStrings("renderer", system.getName())) {
			((Renderer&)system).addPlugin(*this);
		}
	}

	bool resize(FSR2Context& ctx, IVec2 display_size) {
		ffxFsr2ContextDestroy(&ctx.context);

		FfxFsr2ContextDescription initialization_parameters = {};
		ctx.scratch_buffer.resize(ffxFsr2GetScratchMemorySizeDX12());
		ID3D12Device* device = (ID3D12Device*)gpu::getDX12Device();
		FfxErrorCode err = ffxFsr2GetInterfaceDX12(&initialization_parameters.callbacks, device, ctx.scratch_buffer.getMutableData(), ctx.scratch_buffer.size());
		if (err != FFX_OK) return false;
		
		initialization_parameters.device = ffxGetDeviceDX12(device);
		initialization_parameters.flags = FFX_FSR2_ENABLE_AUTO_EXPOSURE;
		initialization_parameters.flags |= FFX_FSR2_ENABLE_DEPTH_INVERTED | FFX_FSR2_ENABLE_DEPTH_INFINITE;
		initialization_parameters.flags |= FFX_FSR2_ENABLE_DEBUG_CHECKING;
		initialization_parameters.flags |= FFX_FSR2_ENABLE_HIGH_DYNAMIC_RANGE;
		initialization_parameters.fpMessage = &onFSR2Msg;

		initialization_parameters.displaySize.width = display_size.x;
		initialization_parameters.displaySize.height = display_size.y;
		initialization_parameters.maxRenderSize.width = display_size.x;
		initialization_parameters.maxRenderSize.height = display_size.y;
		err = ffxFsr2ContextCreate(&ctx.context, &initialization_parameters);
		ctx.size = display_size;

		return err == FFX_OK;
	}

	FSR2Context* getOrCreateContext(Pipeline& pipeline) {
		for (i32 i = 0; i < m_contexts.size(); ++i) {
			if (m_contexts[i]->pipeline == &pipeline) {
				return m_contexts[i].get();
			}
		}

		UniquePtr<FSR2Context> ctx = UniquePtr<FSR2Context>::create(m_allocator, m_allocator);
		ctx->pipeline = &pipeline;

		FfxFsr2ContextDescription initialization_parameters = {};
		ctx->scratch_buffer.resize(ffxFsr2GetScratchMemorySizeDX12());
		ID3D12Device* device = (ID3D12Device*)gpu::getDX12Device();
		FfxErrorCode err = ffxFsr2GetInterfaceDX12(&initialization_parameters.callbacks, device, ctx->scratch_buffer.getMutableData(), ctx->scratch_buffer.size());
		if (err != FFX_OK) return nullptr;
		
		initialization_parameters.device = ffxGetDeviceDX12(device);
		initialization_parameters.flags = FFX_FSR2_ENABLE_AUTO_EXPOSURE;
		initialization_parameters.flags |= FFX_FSR2_ENABLE_DEPTH_INVERTED | FFX_FSR2_ENABLE_DEPTH_INFINITE;
		initialization_parameters.flags |= FFX_FSR2_ENABLE_DEBUG_CHECKING;
		initialization_parameters.flags |= FFX_FSR2_ENABLE_HIGH_DYNAMIC_RANGE;
		initialization_parameters.fpMessage = &onFSR2Msg;

		const IVec2 display_size = pipeline.getDisplaySize();

		initialization_parameters.displaySize.width = display_size.x;
		initialization_parameters.displaySize.height = display_size.y;
		initialization_parameters.maxRenderSize.width = display_size.x;
		initialization_parameters.maxRenderSize.height = display_size.y;
		ctx->size = display_size;
		err = ffxFsr2ContextCreate(&ctx->context, &initialization_parameters);
		if (err != FFX_OK) m_contexts.pop();
		
		m_contexts.push(ctx.move());
		return m_contexts.back().get();
	}

	bool renderAA(Pipeline& pipeline, gpu::TextureHandle color, gpu::TextureHandle velocity, gpu::TextureHandle depth, gpu::TextureHandle output) override {
		FSR2Context* ctx = getOrCreateContext(pipeline);
		if (!ctx) return false;

		pipeline.enablePixelJitter(true);
		Renderer& renderer = pipeline.getRenderer();
		DrawStream& stream = renderer.getDrawStream();
		gpu::FSR2DispatchParams params;
		const Viewport& vp = pipeline.getViewport();
		params.jitter_x = vp.pixel_offset.x;
		params.jitter_y = vp.pixel_offset.y;
		params.time_delta = renderer.getEngine().getLastTimeDelta() * 1000;
		params.render_width = vp.w;
		params.render_height = vp.h;
		params.near_plane = vp.near;
		params.fov = vp.fov;
		params.color = color;
		params.depth = depth;
		params.motion_vectors = velocity;
		params.output = output;
		stream.beginProfileBlock("FSR2", 0);
		IVec2 display_size = pipeline.getDisplaySize();
		stream.pushLambda([this, params, display_size, ctx](){
			if (ctx->size != display_size) resize(*ctx, display_size);
			fsr2Dispatch(*ctx, params);
		});
		stream.endProfileBlock();
		return true;	
	}

	void pipelineDestroyed(Pipeline& pipeline) {
		for (i32 i = 0; i < m_contexts.size(); ++i) {
			if (m_contexts[i]->pipeline == &pipeline) {
				m_contexts.erase(i);
				break;
			}
		}
	}
	
	static void onFSR2Msg(FfxFsr2MsgType type, const wchar_t* message)  {
	
	}

	bool fsr2Dispatch(FSR2Context& ctx, const FSR2DispatchParams& params) {
		FfxFsr2DispatchDescription dispatch_params = {};
		ID3D12CommandList* cmd_list = (ID3D12CommandList*)gpu::getDX12CommandList();
		dispatch_params.commandList = ffxGetCommandListDX12(cmd_list);
    
		dispatch_params.color = ffxGetResourceDX12(&ctx.context, (ID3D12Resource*)gpu::getDX12Resource(params.color), L"FSR2_InputColor");
		dispatch_params.depth = ffxGetResourceDX12(&ctx.context, (ID3D12Resource*)gpu::getDX12Resource(params.depth), L"FSR2_InputDepth");
		dispatch_params.motionVectors = ffxGetResourceDX12(&ctx.context, (ID3D12Resource*)gpu::getDX12Resource(params.motion_vectors), L"FSR2_InputMotionVectors");
		dispatch_params.exposure = ffxGetResourceDX12(&ctx.context, nullptr, L"FSR2_InputExposure");
		dispatch_params.reactive = ffxGetResourceDX12(&ctx.context, nullptr, L"FSR2_EmptyInputReactiveMap");
		dispatch_params.transparencyAndComposition = ffxGetResourceDX12(&ctx.context, nullptr, L"FSR2_EmptyTransparencyAndCompositionMap");
		dispatch_params.output = ffxGetResourceDX12(&ctx.context, (ID3D12Resource*)gpu::getDX12Resource(params.output), L"FSR2_OutputUpscaledColor", FFX_RESOURCE_STATE_UNORDERED_ACCESS);
    
		dispatch_params.jitterOffset.x = params.jitter_x;
		dispatch_params.jitterOffset.y = params.jitter_y;
		dispatch_params.motionVectorScale.x = 0.5f * float(params.render_width);
		dispatch_params.motionVectorScale.y = -0.5f * float(params.render_height);
		dispatch_params.reset = false;
		dispatch_params.enableSharpening = false;
		dispatch_params.sharpness = 0.8f;
		dispatch_params.frameTimeDelta = params.time_delta * 1000.f;
		dispatch_params.preExposure = 1.0f;
		dispatch_params.renderSize.width = params.render_width;
		dispatch_params.renderSize.height = params.render_height;
		dispatch_params.cameraNear = params.near_plane;
		dispatch_params.cameraFovAngleVertical = params.fov;
		const FfxErrorCode err = ffxFsr2ContextDispatch(&ctx.context, &dispatch_params);
		gpu::resetCommandList();
		return err == FFX_OK;
	}

	IAllocator& m_allocator;
	Array<UniquePtr<FSR2Context>> m_contexts;
};

LUMIX_PLUGIN_ENTRY(fsr2) {
	return LUMIX_NEW(engine.getAllocator(), FSR2RendererPlugin)(engine);
}


} // namespace Lumix::gpu