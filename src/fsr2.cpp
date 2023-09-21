#include "engine/stream.h"
#include "fsr2.h"

#include "ffx_fsr2.h"
#include "dx12/ffx_fsr2_dx12.h"
#pragma comment(lib, "ffx_fsr2_api_dx12_x64d.lib")
#pragma comment(lib, "ffx_fsr2_api_x64d.lib")

namespace Lumix::gpu {

void* getDX12CommandList();
void* getDX12Device();
void* getDX12Resource(TextureHandle h);
void resetCommandList();

struct FSR2Context {
	FSR2Context(IAllocator& allocator) : allocator(allocator), scratch_buffer(allocator) {}

	IAllocator& allocator;
	FfxFsr2Context context;
	OutputMemoryStream scratch_buffer;
};

static void onFSR2Msg(FfxFsr2MsgType type, const wchar_t* message)  {
	
}

FSR2Context* fsr2Init(u32 w, u32 h, IAllocator& allocator) {
	FSR2Context* ctx = LUMIX_NEW(allocator, FSR2Context)(allocator);

	FfxFsr2ContextDescription initialization_parameters = {};
	ctx->scratch_buffer.resize(ffxFsr2GetScratchMemorySizeDX12());
	ID3D12Device* device = (ID3D12Device*)gpu::getDX12Device();
	FfxErrorCode err = ffxFsr2GetInterfaceDX12(&initialization_parameters.callbacks, device, ctx->scratch_buffer.getMutableData(), ctx->scratch_buffer.size());
	if (err != FFX_OK) {
		LUMIX_DELETE(allocator, ctx);
		return nullptr;
	}
		
	initialization_parameters.device = ffxGetDeviceDX12(device);
	initialization_parameters.flags = FFX_FSR2_ENABLE_AUTO_EXPOSURE;
	initialization_parameters.flags |= FFX_FSR2_ENABLE_DEPTH_INVERTED | FFX_FSR2_ENABLE_DEPTH_INFINITE;
	initialization_parameters.flags |= FFX_FSR2_ENABLE_DEBUG_CHECKING;
	//initialization_parameters.flags |= FFX_FSR2_ENABLE_MOTION_VECTORS_JITTER_CANCELLATION;
	initialization_parameters.flags |= FFX_FSR2_ENABLE_HIGH_DYNAMIC_RANGE;
	initialization_parameters.fpMessage = &onFSR2Msg;

	initialization_parameters.displaySize.width = w;
	initialization_parameters.displaySize.height = h;
	initialization_parameters.maxRenderSize.width = w;
	initialization_parameters.maxRenderSize.height = h;
	err = ffxFsr2ContextCreate(&ctx->context, &initialization_parameters);
	if (err != FFX_OK) {
		LUMIX_DELETE(allocator, ctx);
		return nullptr;
	}
	return ctx;
}

void fsr2Shutdown(FSR2Context& ctx) {
	LUMIX_DELETE(ctx.allocator, &ctx);
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
    dispatch_params.motionVectorScale.x = -float(params.render_width);
    dispatch_params.motionVectorScale.y = -float(params.render_height);
    dispatch_params.reset = false;
    dispatch_params.enableSharpening = false;
    dispatch_params.sharpness = 0;
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

} // namespace Lumix::gpu