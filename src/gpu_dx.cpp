#include "renderer/gpu/gpu.h"
#include "../external/include/glslang/Public/ShaderLang.h"
#include "../external/include/SPIRV/GlslangToSpv.h"
#include "../external/include/spirv_cross/spirv_hlsl.hpp"
#include "engine/array.h"
#include "engine/crc32.h"
#include "engine/hash_map.h"
#include "engine/log.h"
#include "engine/math.h"
#include "engine/os.h"
#include "engine/sync.h"
#include "engine/stream.h"
#include "shader_compiler.h"
#include <Windows.h>
#include <d3d11_1.h>
#include <dxgi.h>
#include <dxgi1_6.h>
#include <cassert>
#include <malloc.h>
#include "renderer/gpu/dds.h"
#include "renderer/gpu/renderdoc_app.h"
#include "stb/stb_image_resize.h"

#pragma comment(lib, "glslang.lib")
#pragma comment(lib, "OSDependent.lib")
#pragma comment(lib, "OGLCompiler.lib")
#pragma comment(lib, "HLSL.lib")
#pragma comment(lib, "SPIRV.lib")
#pragma comment(lib, "spirv-cross-core.lib")
#pragma comment(lib, "spirv-cross-cpp.lib")
#pragma comment(lib, "spirv-cross-glsl.lib")
#pragma comment(lib, "spirv-cross-hlsl.lib")

namespace Lumix {

namespace gpu {


template <int N>
static void toWChar(WCHAR (&out)[N], const char* in)
{
	const char* c = in;
	WCHAR* cout = out;
	while (*c && c - in < N - 1) {
		*cout = *c;
		++cout;
		++c;
	}
	*cout = 0;
}

static DXGI_FORMAT getDXGIFormat(const Attribute& attr) {
	switch (attr.type) {
		case AttributeType::FLOAT: 
			switch(attr.components_count) {
				case 1: return DXGI_FORMAT_R32_FLOAT;
				case 2: return DXGI_FORMAT_R32G32_FLOAT;
				case 3: return DXGI_FORMAT_R32G32B32_FLOAT;
				case 4: return DXGI_FORMAT_R32G32B32A32_FLOAT;
			}
			break;
		case AttributeType::I8: 
			switch(attr.components_count) {
				case 1: return DXGI_FORMAT_R8_SNORM;
				case 2: return DXGI_FORMAT_R8G8_SNORM;
				case 4: return DXGI_FORMAT_R8G8B8A8_SNORM;
			}
			break;
		case AttributeType::U8: 
			switch(attr.components_count) {
				case 1: return DXGI_FORMAT_R8_UNORM;
				case 2: return DXGI_FORMAT_R8G8_UNORM;
				case 4: return DXGI_FORMAT_R8G8B8A8_UNORM;
			}
			break;
		case AttributeType::I16: 
			switch(attr.components_count) {
				case 4: return DXGI_FORMAT_R16G16B16A16_SINT;
			}
			break;
	}
	ASSERT(false);
	return DXGI_FORMAT_R32_FLOAT;
}

static u32 getSize(DXGI_FORMAT format) {
	switch(format) {
		case DXGI_FORMAT_R8_UNORM: return 1;
		case DXGI_FORMAT_R32_TYPELESS: return 4;
		case DXGI_FORMAT_R24G8_TYPELESS: return 4;
		case DXGI_FORMAT_R8G8B8A8_UNORM_SRGB: return 4;
		case DXGI_FORMAT_R8G8B8A8_UNORM: return 4;
		case DXGI_FORMAT_R16G16B16A16_UNORM: return 8;
		case DXGI_FORMAT_R16G16B16A16_FLOAT: return 8;
		case DXGI_FORMAT_R32G32_FLOAT: return 8;
		case DXGI_FORMAT_R32G32B32_FLOAT: return 12;
		case DXGI_FORMAT_R32G32B32A32_FLOAT: return 16;
		case DXGI_FORMAT_R16_UNORM: return 2;
		case DXGI_FORMAT_R16_FLOAT: return 2;
		case DXGI_FORMAT_R32_FLOAT: return 4;
	}
	ASSERT(false);
	return 0;
}

int getSize(AttributeType type) {
	switch(type) {
		case AttributeType::FLOAT: return 4;
		case AttributeType::U8: return 1;
		case AttributeType::I8: return 1;
		case AttributeType::I16: return 2;
		default: ASSERT(false); return 0;
	}
}

static DXGI_FORMAT getDXGIFormat(TextureFormat format) {
	switch (format) {
		case TextureFormat::R8: return DXGI_FORMAT_R8_UNORM;
		case TextureFormat::D32: return DXGI_FORMAT_R32_TYPELESS;
		case TextureFormat::D24: return DXGI_FORMAT_R32_TYPELESS;
		case TextureFormat::D24S8: return DXGI_FORMAT_R24G8_TYPELESS;
		//case TextureFormat::SRGB: return DXGI_FORMAT_R32_FLOAT;
		case TextureFormat::SRGBA: return DXGI_FORMAT_R8G8B8A8_UNORM_SRGB;
		case TextureFormat::RGBA8: return DXGI_FORMAT_R8G8B8A8_UNORM;
		case TextureFormat::RGBA16: return DXGI_FORMAT_R16G16B16A16_UNORM;
		case TextureFormat::RGBA16F: return DXGI_FORMAT_R16G16B16A16_FLOAT;
		case TextureFormat::RGBA32F: return DXGI_FORMAT_R32G32B32A32_FLOAT;
		case TextureFormat::R16: return DXGI_FORMAT_R16_UNORM;
		case TextureFormat::R16F: return DXGI_FORMAT_R16_FLOAT;
		case TextureFormat::R32F: return DXGI_FORMAT_R32_FLOAT;
		case TextureFormat::RG32F: return DXGI_FORMAT_R32G32_FLOAT;
	}
	ASSERT(false);
	return DXGI_FORMAT_R8G8B8A8_UINT;
}


struct Program {
	~Program() {
		if (gs) gs->Release();
		if (ps) ps->Release();
		if (vs) vs->Release();
		if (cs) cs->Release();
		if (il) il->Release();
	}

	ID3D11VertexShader* vs = nullptr;
	ID3D11PixelShader* ps = nullptr;
	ID3D11GeometryShader* gs = nullptr;
	ID3D11ComputeShader* cs = nullptr;
	ID3D11InputLayout* il = nullptr;
	#ifdef LUMIX_DEBUG
		StaticString<64> name;
	#endif
};

struct Buffer {
	~Buffer() {
		if (srv) srv->Release();
		if (uav) uav->Release();
		buffer->Release();
	}

	ID3D11Buffer* buffer = nullptr;
	ID3D11ShaderResourceView* srv = nullptr;
	ID3D11UnorderedAccessView* uav = nullptr;
	u8* mapped_ptr = nullptr;
	bool is_constant_buffer = false;
	u32 bound_to_output = 0; 
};

struct Texture {
	~Texture() {
		if (srv) srv->Release();
		if (dsv_ro) dsv_ro->Release();
		if (dsv) dsv->Release();
		if (rtv) rtv->Release();
		if (uav) uav->Release();
		if (texture2D) texture2D->Release();
		if (texture3D) texture3D->Release();
	}

	ID3D11Texture2D* texture2D = nullptr;
	ID3D11Texture3D* texture3D = nullptr;
	ID3D11RenderTargetView* rtv = nullptr;
	ID3D11DepthStencilView* dsv = nullptr;
	ID3D11DepthStencilView* dsv_ro = nullptr;
	ID3D11UnorderedAccessView* uav = nullptr;
	ID3D11ShaderResourceView* srv = nullptr;
	ID3D11SamplerState* sampler = nullptr;
	u32 rtv_face = 0;
	u32 rtv_mip = 0;
	u32 flags;
	u32 w;
	u32 h;
	DXGI_FORMAT dxgi_format;
	u32 bound_to_output = 0xffFFffFF;
	u32 bound_to_input = 0xffFFffFF;
	#ifdef LUMIX_DEBUG
		StaticString<64> name;
	#endif
};

struct Query {
	~Query() {
		if (query) query->Release();
	}
	ID3D11Query* query;
};

struct InputLayout {
	ID3D11InputLayout* layout;
};

struct ShaderCompilerDX11 : ShaderCompiler {
	ShaderCompilerDX11(IAllocator& allocator)
		: ShaderCompiler(allocator)
	{}

	static bool create(ID3D11Device* device, ShaderType type, const void* ptr, size_t len, Ref<Program> program) {
		HRESULT hr;
		switch(type) {
			case ShaderType::VERTEX: hr = device->CreateVertexShader(ptr, len, nullptr, &program->vs); break;
			case ShaderType::FRAGMENT: hr = device->CreatePixelShader(ptr, len, nullptr, &program->ps); break;
			case ShaderType::GEOMETRY: hr = device->CreateGeometryShader(ptr, len, nullptr, &program->gs); break;
			case ShaderType::COMPUTE: hr = device->CreateComputeShader(ptr, len, nullptr, &program->cs); break;
			default: ASSERT(false); break;
		}
		return SUCCEEDED(hr);
	}

	bool compile(ID3D11Device* device
		, const Input& input
		, const char* name
		, Ref<Program> program)
	{
		auto compile_stage = [&](ShaderType type){
			const char* tmp[128];
			const u32 c = filter(input, type, tmp);
			if (c == 0) return true;
			if (c > (u32)input.prefixes.length() + input.decl.attributes_count) {
				const u32 hash = computeHash(tmp, c);
				auto iter = m_cache.find(hash);
				if (iter.isValid()) {
					const ShaderCompiler::CachedShader& s = iter.value();
					if (!create(device, type, s.data.data(), s.data.size(), program)) return false;
					if (type == ShaderType::VERTEX) {
						createInputLayout(device, input.decl, s.data.data(), s.data.size(), program);
					}
					return true;
				}
				std::string hlsl;
				u32 dummy;
				if (!glsl2hlsl(tmp, c, type, name, Ref(hlsl), Ref(dummy), Ref(dummy))) {
					return false;
				}
				ID3DBlob* blob = ShaderCompiler::compile(hash, hlsl.c_str(), type, name, 0, 0);
				if (!blob) return false;
				if (!create(device, type, blob->GetBufferPointer(), blob->GetBufferSize(), program)) return false;
				if (type == ShaderType::VERTEX) {
					createInputLayout(device, input.decl, blob->GetBufferPointer(), blob->GetBufferSize(), program);
				}
				blob->Release();
				return true;
			}
			return false;
		};

		bool compiled = compile_stage(ShaderType::VERTEX);
		compiled = compiled && compile_stage(ShaderType::FRAGMENT);
		compiled = compiled && compile_stage(ShaderType::COMPUTE);
		compiled = compiled && compile_stage(ShaderType::GEOMETRY);
		if (!compiled) return false;

		if (name && name[0]) {
			if(program->vs) program->vs->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(name), name);
			if(program->ps) program->ps->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(name), name);
			if(program->gs) program->gs->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(name), name);
			if(program->cs) program->cs->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(name), name);
		}

		return true;
	}

	void createInputLayout(ID3D11Device* device
		, const VertexDecl& decl
		, const void* bytecode
		, size_t bytecode_size
		, Ref<Program> program)
	{
		D3D11_INPUT_ELEMENT_DESC descs[16];
		for (u8 i = 0; i < decl.attributes_count; ++i) {
			const Attribute& attr = decl.attributes[i];
			const bool instanced = attr.flags & Attribute::INSTANCED;
			descs[i].AlignedByteOffset = attr.byte_offset;
			descs[i].Format = getDXGIFormat(attr);
			descs[i].SemanticIndex = attr.idx;
			descs[i].SemanticName = "TEXCOORD";
			descs[i].InputSlot = instanced ? 1 : 0;
			descs[i].InputSlotClass = instanced ? D3D11_INPUT_PER_INSTANCE_DATA : D3D11_INPUT_PER_VERTEX_DATA; 
			descs[i].InstanceDataStepRate = instanced ? 1 : 0;
		}

		if (bytecode && decl.attributes_count > 0) {
			device->CreateInputLayout(descs, decl.attributes_count, bytecode, bytecode_size, &program->il);
		}
		else {
			program->il = nullptr;
		}
	}
};

static struct D3D {
	bool initialized = false;

	struct FrameBuffer {
		ID3D11DepthStencilView* depth_stencil = nullptr;
		ID3D11RenderTargetView* render_targets[16];
		u32 count = 0;
	};

	struct Window { 
		void* handle = nullptr;
		IDXGISwapChain* swapchain = nullptr;
		FrameBuffer framebuffer;
		IVec2 size = IVec2(800, 600);
	};
	
	struct State {
		ID3D11DepthStencilState* dss;
		ID3D11RasterizerState* rs;
		ID3D11BlendState* bs;
	};

	D3D() 
		: state_cache(static_allocator)
		, shader_compiler(static_allocator)
	{}

	DWORD thread;
	RENDERDOC_API_1_0_2* rdoc_api;
	IAllocator* allocator = nullptr;
	ID3D11DeviceContext1* device_ctx = nullptr;
	TextureHandle bound_image_textures[16];
	TextureHandle bound_textures[16];
	void* bound_uavs[16];
	ID3D11Device* device = nullptr;
	ID3D11Debug* debug = nullptr;
	ID3DUserDefinedAnnotation* annotation = nullptr;
	ID3D11Query* disjoint_query = nullptr;
	bool disjoint_waiting = false;
	u64 query_frequency = 1;

	BufferHandle current_index_buffer = INVALID_BUFFER;
	BufferHandle current_indirect_buffer = INVALID_BUFFER;
	Window windows[64];
	Window* current_window = windows;
	ID3D11SamplerState* samplers[2*2*2*2];

	FrameBuffer current_framebuffer;

	DefaultAllocator static_allocator;
	HashMap<u64, State> state_cache;
	HMODULE d3d_dll;
	HMODULE dxgi_dll;
	ProgramHandle current_program = nullptr;
	ShaderCompilerDX11 shader_compiler;	
	#ifdef LUMIX_DEBUG
		StaticString<64> debug_group;
	#endif
} d3d;

namespace DDS
{

static u32 sizeDXTC(u32 w, u32 h, DXGI_FORMAT format) {
	const bool is_dxt1 = format == DXGI_FORMAT_BC1_UNORM || format == DXGI_FORMAT_BC1_UNORM_SRGB;
	const bool is_ati = format == DXGI_FORMAT_BC4_UNORM;
	return ((w + 3) / 4) * ((h + 3) / 4) * (is_dxt1 || is_ati ? 8 : 16);
}

struct LoadInfo {
	bool compressed;
	bool swap;
	bool palette;
	u32 blockBytes;
	u32 block_width;
	u32 block_height;
	DXGI_FORMAT format;
	DXGI_FORMAT srgb_format;
};

static LoadInfo loadInfoDXT1 = {
	true, false, false, 8, 4, 4, DXGI_FORMAT_BC1_UNORM, DXGI_FORMAT_BC1_UNORM_SRGB
};
static LoadInfo loadInfoDXT3 = {
	true, false, false, 16, 4, 4, DXGI_FORMAT_BC2_UNORM, DXGI_FORMAT_BC2_UNORM_SRGB
};
static LoadInfo loadInfoDXT5 = {
	true, false, false, 16, 4, 4, DXGI_FORMAT_BC3_UNORM, DXGI_FORMAT_BC3_UNORM_SRGB
};
static LoadInfo loadInfoATI1 = {
	true, false, false, 8, 4, 4, DXGI_FORMAT_BC4_UNORM, DXGI_FORMAT_UNKNOWN
};
static LoadInfo loadInfoATI2 = {
	true, false, false, 16, 4, 4, DXGI_FORMAT_BC5_UNORM, DXGI_FORMAT_UNKNOWN
};
static LoadInfo loadInfoBGRA8 = {
//	false, false, false, 4, GL_RGBA8, GL_SRGB8_ALPHA8, GL_BGRA, GL_UNSIGNED_BYTE
};
static LoadInfo loadInfoRGBA8 = {
//	false, false, false, 4, GL_RGBA8, GL_SRGB8_ALPHA8, GL_RGBA, GL_UNSIGNED_BYTE
};
static LoadInfo loadInfoBGR8 = {
//	false, false, false, 3, GL_RGB8, GL_SRGB8, GL_BGR, GL_UNSIGNED_BYTE
};
static LoadInfo loadInfoBGR5A1 = {
//	false, true, false, 2, GL_RGB5_A1, GL_ZERO, GL_BGRA, GL_UNSIGNED_SHORT_1_5_5_5_REV
};
static LoadInfo loadInfoBGR565 = {
//	false, true, false, 2, GL_RGB5, GL_ZERO, GL_RGB, GL_UNSIGNED_SHORT_5_6_5
};
static LoadInfo loadInfoIndex8 = {
//	false, false, true, 1, GL_RGB8, GL_SRGB8, GL_BGRA, GL_UNSIGNED_BYTE
};

static LoadInfo* getDXT10LoadInfo(const Header& hdr, const DXT10Header& dxt10_hdr)
{
	switch(dxt10_hdr.dxgi_format) {
		case DxgiFormat::B8G8R8A8_UNORM_SRGB:
		case DxgiFormat::B8G8R8A8_UNORM:
			return &loadInfoBGRA8;
			break;
		case DxgiFormat::R8G8B8A8_UNORM:
			return &loadInfoRGBA8;
			break;
		case DxgiFormat::BC1_UNORM_SRGB:
		case DxgiFormat::BC1_UNORM:
			return &loadInfoDXT1;
			break;
		case DxgiFormat::BC2_UNORM_SRGB:
		case DxgiFormat::BC2_UNORM:
			return &loadInfoDXT3;
			break;
		case DxgiFormat::BC3_UNORM_SRGB:
		case DxgiFormat::BC3_UNORM:
			return &loadInfoDXT5;
			break;
		default:
			ASSERT(false);
			return nullptr;
			break;
	}
}


} // namespace DDS

static void try_load_renderdoc() {
	HMODULE lib = LoadLibrary("renderdoc.dll");
	if (!lib) lib = LoadLibrary("C:\\Program Files\\RenderDoc\\renderdoc.dll");
	if (!lib) return;
	pRENDERDOC_GetAPI RENDERDOC_GetAPI = (pRENDERDOC_GetAPI)GetProcAddress(lib, "RENDERDOC_GetAPI");
	if (RENDERDOC_GetAPI) {
		RENDERDOC_GetAPI(eRENDERDOC_API_Version_1_0_2, (void **)&d3d.rdoc_api);
		d3d.rdoc_api->MaskOverlayBits(~RENDERDOC_OverlayBits::eRENDERDOC_Overlay_Enabled, 0);
	}
	/**/
	//FreeLibrary(lib);
}

void launchRenderDoc() {
	if (d3d.rdoc_api) {
		d3d.rdoc_api->LaunchReplayUI(1, "");
	}
}

static bool isDepthFormat(DXGI_FORMAT format) {
	switch(format) {
		case DXGI_FORMAT_R24G8_TYPELESS: return true;
		case DXGI_FORMAT_R32_TYPELESS: return true;
	}
	return false;
}

static DXGI_FORMAT toViewFormat(DXGI_FORMAT format) {
	switch(format) {
		case DXGI_FORMAT_R24G8_TYPELESS: return DXGI_FORMAT_R24_UNORM_X8_TYPELESS;
		case DXGI_FORMAT_R32_TYPELESS: return DXGI_FORMAT_R32_FLOAT;
	}
	return format;
}

static DXGI_FORMAT toDSViewFormat(DXGI_FORMAT format) {
	switch(format) {
		case DXGI_FORMAT_R24G8_TYPELESS: return DXGI_FORMAT_D24_UNORM_S8_UINT;
		case DXGI_FORMAT_R32_TYPELESS: return DXGI_FORMAT_D32_FLOAT;
	}
	return format;
}

QueryHandle createQuery() {
	checkThread();

	Query* q = LUMIX_NEW(*d3d.allocator, Query);
	*q = {};
	D3D11_QUERY_DESC desc = {};
	desc.Query = D3D11_QUERY_TIMESTAMP;
	d3d.device->CreateQuery(&desc, &q->query);
	ASSERT(q->query);
	return { q }; 
}

void startCapture()
{
	if (d3d.rdoc_api) {
		d3d.rdoc_api->StartFrameCapture(nullptr, nullptr);
	}
}


void stopCapture() {
	if (d3d.rdoc_api) {
		d3d.rdoc_api->EndFrameCapture(nullptr, nullptr);
	}
}

void checkThread() {
	ASSERT(d3d.thread == GetCurrentThreadId());
}

void destroy(ProgramHandle program) {
	checkThread();
	
	LUMIX_DELETE(*d3d.allocator, program)
}

void destroy(TextureHandle texture) {
	checkThread();
	LUMIX_DELETE(*d3d.allocator, texture);
}

void destroy(QueryHandle query) {
	checkThread();
	LUMIX_DELETE(*d3d.allocator, query);
}

void drawTriangleStripArraysInstanced(u32 indices_count, u32 instances_count) {
	d3d.device_ctx->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
	d3d.device_ctx->DrawInstanced(indices_count, instances_count, 0, 0);
}

void createTextureView(TextureHandle view, TextureHandle texture) {
	ASSERT(view);
	ASSERT(texture);

	view->dxgi_format = texture->dxgi_format;
	view->sampler = texture->sampler;
	D3D11_SHADER_RESOURCE_VIEW_DESC srv_desc = {};
	texture->srv->GetDesc(&srv_desc);
	if (srv_desc.ViewDimension != D3D_SRV_DIMENSION_TEXTURE2D) {
		srv_desc.ViewDimension = D3D_SRV_DIMENSION_TEXTURE2D;
	}

	d3d.device->CreateShaderResourceView(texture->texture2D, &srv_desc, &view->srv);
}

void generateMipmaps(TextureHandle texture){
	ASSERT(texture);
	d3d.device_ctx->GenerateMips(texture->srv);
}

void update(TextureHandle texture, u32 mip, u32 face, u32 x, u32 y, u32 w, u32 h, TextureFormat format, void* buf) {
	ASSERT(texture);
	ASSERT(texture->dxgi_format == getDXGIFormat(format));

	const bool no_mips = texture->flags & (u32)TextureFlags::NO_MIPS;
	const u32 mip_count = no_mips ? 1 : 1 + log2(maximum(texture->w, texture->h));
	const UINT subres = D3D11CalcSubresource(mip, face, mip_count);
	const u32 bytes_per_pixel = getSize(texture->dxgi_format);
	const UINT row_pitch = w * bytes_per_pixel;
	const UINT depth_pitch = row_pitch * h;
	D3D11_BOX box;
	box.left = x;
	box.top = y;
	box.right = x + w;
	box.bottom = y + h;
	box.front = 0;
	box.back = 1;
	
	d3d.device_ctx->UpdateSubresource(texture->texture2D, subres, &box, buf, row_pitch, depth_pitch);
}

void copy(TextureHandle dst, TextureHandle src, u32 dst_x, u32 dst_y) {
	ASSERT(dst);
	ASSERT(src);

	const bool no_mips = src->flags & (u32)TextureFlags::NO_MIPS;
	const u32 src_mip_count = no_mips ? 1 : 1 + log2(maximum(src->w, src->h));
	const u32 dst_mip_count = no_mips ? 1 : 1 + log2(maximum(dst->w, dst->h));

	u32 mip = 0;
	while ((src->w >> mip) != 0 || (src->h >> mip) != 0) {
		const u32 w = maximum(src->w >> mip, 1);
		const u32 h = maximum(src->h >> mip, 1);

		if (src->flags & (u32)TextureFlags::IS_CUBE) {
			for (u32 face = 0; face < 6; ++face) {
				const UINT src_subres = D3D11CalcSubresource(mip, face, src_mip_count);
				const UINT dst_subres = D3D11CalcSubresource(mip, face, dst_mip_count);
				d3d.device_ctx->CopySubresourceRegion(dst->texture2D, dst_subres, dst_x, dst_y, 0, src->texture2D, src_subres, nullptr);
			}
		}
		else {
			const UINT src_subres = D3D11CalcSubresource(mip, 0, src_mip_count);
			const UINT dst_subres = D3D11CalcSubresource(mip, 0, dst_mip_count);
			d3d.device_ctx->CopySubresourceRegion(dst->texture2D, dst_subres, dst_x, dst_y, 0, src->texture2D, src_subres, nullptr);
		}
		++mip;
		if (src->flags & (u32)TextureFlags::NO_MIPS) break;
		if (dst->flags & (u32)TextureFlags::NO_MIPS) break;
	}
}

void readTexture(TextureHandle texture, u32 mip, Span<u8> buf) {
	ASSERT(texture);
	D3D11_MAPPED_SUBRESOURCE data;
	
	const u32 faces = (texture->flags & (u32)TextureFlags::IS_CUBE) ? 6 : 1;
	const bool no_mips = texture->flags & (u32)TextureFlags::NO_MIPS;
	const u32 mip_count = no_mips ? 1 : 1 + log2(maximum(texture->w, texture->h));
	u8* ptr = buf.begin();
	
	for (u32 face = 0; face < faces; ++face) {
		const UINT subres = D3D11CalcSubresource(mip, face, mip_count);
		d3d.device_ctx->Map(texture->texture2D, subres, D3D11_MAP_READ, 0, &data);
		ASSERT(data.DepthPitch == buf.length() / faces);
		memcpy(ptr, data.pData, data.DepthPitch);
		ptr += data.DepthPitch;
		d3d.device_ctx->Unmap(texture->texture2D, subres);
	}
}

void queryTimestamp(QueryHandle query) {
	checkThread();
	ASSERT(query);
	d3d.device_ctx->End(query->query);
}

u64 getQueryFrequency() { return d3d.query_frequency; }

u64 getQueryResult(QueryHandle query) {
	checkThread();
	ASSERT(query);
	ID3D11Query* q = query->query;
	u64 time;
	const HRESULT res = d3d.device_ctx->GetData(q, &time, sizeof(time), 0);
	ASSERT(res == S_OK);
	return time;
}

bool isQueryReady(QueryHandle query) { 
	checkThread();
	ASSERT(query);
	ID3D11Query* q = query->query;
	const HRESULT res = d3d.device_ctx->GetData(q, nullptr, 0, 0);
	return res == S_OK;
}

void preinit(IAllocator& allocator, bool load_renderdoc)
{
	if (load_renderdoc) try_load_renderdoc();
	d3d.allocator = &allocator;
}

void shutdown() {
	d3d.shader_compiler.save(".shader_cache_dx11");

	ShFinalize();

	for (const D3D::State& s : d3d.state_cache) {
		s.bs->Release();
		s.dss->Release();
		s.rs->Release();
	}
	d3d.state_cache.clear();

	for (ID3D11SamplerState*& sampler : d3d.samplers) {
		if (!sampler) continue;

		sampler->Release();
		sampler = nullptr;
	}

	for (D3D::Window& w : d3d.windows) {
		if (!w.handle) continue;

		if (w.framebuffer.depth_stencil) {
			w.framebuffer.depth_stencil->Release();
			w.framebuffer.depth_stencil = nullptr;
		}
		for (u32 i = 0; i < w.framebuffer.count; ++i) {
			w.framebuffer.render_targets[i]->Release();
		}
		w.framebuffer.count = 0;
		w.swapchain->Release();
	}

	d3d.disjoint_query->Release();
	d3d.annotation->Release();
	d3d.device_ctx->Release();

	#ifdef LUMIX_DEBUG
		if(d3d.debug) d3d.debug->ReportLiveDeviceObjects(D3D11_RLDO_DETAIL | D3D11_RLDO_IGNORE_INTERNAL);
	#endif
	if(d3d.debug) d3d.debug->Release();

	d3d.device->Release();
	FreeLibrary(d3d.d3d_dll);
	FreeLibrary(d3d.dxgi_dll);
}

bool init(void* hwnd, u32 flags) {
	if (d3d.initialized) {
		// we don't support reinitialization
		ASSERT(false);
		return false;
	}

	bool debug = flags & (u32)InitFlags::DEBUG_OUTPUT;
	#ifdef LUMIX_DEBUG
		debug = true;
	#endif

	d3d.thread = GetCurrentThreadId();
	ShInitialize();

	RECT rect;
	GetClientRect((HWND)hwnd, &rect);
	d3d.windows[0].size = IVec2(rect.right - rect.left, rect.bottom - rect.top);
	d3d.windows[0].handle = hwnd;
	d3d.current_window = &d3d.windows[0];

	const int width = rect.right - rect.left;
	const int height = rect.bottom - rect.top;

	d3d.d3d_dll = LoadLibrary("d3d11.dll");
	d3d.dxgi_dll = LoadLibrary("dxgi.dll");
	if (!d3d.d3d_dll) {
		logError("gpu") << "Failed to load d3d11.dll";
		return false;
	}
	if (!d3d.dxgi_dll) {
		logError("gpu") << "Failed to load dxgi.dll";
		return false;
	}

	#define DECL_D3D_API(f) \
		auto api_##f = (decltype(f)*)GetProcAddress(d3d.d3d_dll, #f);
	
	DECL_D3D_API(D3D11CreateDeviceAndSwapChain);
	
	DXGI_SWAP_CHAIN_DESC desc = {};
	desc.BufferDesc.Width = width;
	desc.BufferDesc.Height = height;
	desc.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
	desc.BufferDesc.RefreshRate.Numerator = 60;
	desc.BufferDesc.RefreshRate.Denominator = 1;
	desc.OutputWindow = (HWND)hwnd;
	desc.Windowed = true;
	desc.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
	desc.BufferCount = 1;
	desc.SampleDesc.Count = 1;
	desc.SampleDesc.Quality = 0;
	desc.Flags = DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH;
	desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
	const u32 create_flags = D3D11_CREATE_DEVICE_SINGLETHREADED | (debug ? D3D11_CREATE_DEVICE_DEBUG : 0);
	D3D_FEATURE_LEVEL feature_level;
	ID3D11DeviceContext* ctx;
	HRESULT hr = api_D3D11CreateDeviceAndSwapChain(NULL
		, D3D_DRIVER_TYPE_HARDWARE
		, NULL
		, create_flags
		, NULL
		, 0
		, D3D11_SDK_VERSION
		, &desc
		, &d3d.windows[0].swapchain
		, &d3d.device
		, &feature_level
		, &ctx);

	ctx->QueryInterface(IID_PPV_ARGS(&d3d.device_ctx));
	ctx->Release();

	if(!SUCCEEDED(hr)) return false;

	ID3D11Texture2D* rt;
	hr = d3d.windows[0].swapchain->GetBuffer(0, IID_ID3D11Texture2D, (void**)&rt);
	if(!SUCCEEDED(hr)) return false;

	hr = d3d.device->CreateRenderTargetView((ID3D11Resource*)rt, NULL, &d3d.windows[0].framebuffer.render_targets[0]);
	rt->Release();
	if(!SUCCEEDED(hr)) return false;
	d3d.windows[0].framebuffer.count = 1;
	
	D3D11_TEXTURE2D_DESC ds_desc;
	memset(&ds_desc, 0, sizeof(ds_desc));
	ds_desc.Width = width;
	ds_desc.Height = height;
	ds_desc.MipLevels = 1;
	ds_desc.ArraySize = 1;
	ds_desc.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
	ds_desc.SampleDesc = desc.SampleDesc;
	ds_desc.Usage = D3D11_USAGE_DEFAULT;
	ds_desc.BindFlags = D3D11_BIND_DEPTH_STENCIL;

	ID3D11Texture2D* ds;
	hr = d3d.device->CreateTexture2D(&ds_desc, NULL, &ds);
	if(!SUCCEEDED(hr)) return false;

	ds->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen("default_win_ds"), "default_win_ds");

	D3D11_DEPTH_STENCIL_VIEW_DESC dsv_desc;
	memset(&dsv_desc, 0, sizeof(dsv_desc));
	dsv_desc.Format = ds_desc.Format;
	dsv_desc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;

	hr = d3d.device->CreateDepthStencilView((ID3D11Resource*)ds, &dsv_desc, &d3d.windows[0].framebuffer.depth_stencil);
	if(!SUCCEEDED(hr)) return false;
	ds->Release();

	d3d.current_framebuffer = d3d.windows[0].framebuffer;

	d3d.device_ctx->QueryInterface(IID_PPV_ARGS(&d3d.annotation));

	if(debug) {
		hr = d3d.device->QueryInterface(IID_PPV_ARGS(&d3d.debug));
		if (SUCCEEDED(hr)) {
			ID3D11InfoQueue* info_queue;
			hr = d3d.debug->QueryInterface(IID_PPV_ARGS(&info_queue));
			if (SUCCEEDED(hr)) {
				info_queue->SetBreakOnSeverity(D3D11_MESSAGE_SEVERITY_ERROR, true);
				info_queue->SetBreakOnSeverity(D3D11_MESSAGE_SEVERITY_CORRUPTION, true);
				info_queue->SetBreakOnSeverity(D3D11_MESSAGE_SEVERITY_WARNING, false);
				info_queue->Release();
			}
		}

	}

	D3D11_QUERY_DESC query_desc = {};
	query_desc.Query = D3D11_QUERY_TIMESTAMP_DISJOINT;
	d3d.device->CreateQuery(&query_desc, &d3d.disjoint_query);
	d3d.device_ctx->Begin(d3d.disjoint_query);
	d3d.device_ctx->End(d3d.disjoint_query);
	u32 try_num = 0;
	while (S_OK != d3d.device_ctx->GetData(d3d.disjoint_query, nullptr, 0, 0) && try_num < 1000) {
		Sleep(1);
		++try_num;
	}
	if(try_num == 1000) {
		logError("gpu") << "Failed to get GPU query frequency. All timings are unreliable.";
		d3d.query_frequency = 1'000'000'000;
	}
	else {
		D3D11_QUERY_DATA_TIMESTAMP_DISJOINT data;
		d3d.device_ctx->GetData(d3d.disjoint_query, &data, sizeof(data), 0);
		d3d.query_frequency = data.Frequency;
	}

	d3d.device_ctx->Begin(d3d.disjoint_query);
	d3d.disjoint_waiting = false;

	d3d.shader_compiler.load(".shader_cache_dx11");

	d3d.initialized = true;
	return true;
}

void pushDebugGroup(const char* msg)
{
	WCHAR tmp[128];
	toWChar(tmp, msg);
	d3d.annotation->BeginEvent(tmp);
	#ifdef LUMIX_DEBUG
		d3d.debug_group = msg;
	#endif
}

void popDebugGroup()
{
	d3d.annotation->EndEvent();
}

void setFramebufferCube(TextureHandle cube, u32 face, u32 mip)
{
	ASSERT(cube);
	d3d.current_framebuffer.count = 0;
	d3d.current_framebuffer.depth_stencil = nullptr;
	if (cube->rtv && (cube->rtv_face != face || cube->rtv_mip) != mip) {
		cube->rtv->Release();
		cube->rtv = nullptr;
	}
	if(!cube->rtv) {
		D3D11_RENDER_TARGET_VIEW_DESC desc = {};
		desc.Format = cube->dxgi_format;
		desc.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2DARRAY;
		desc.Texture2DArray.MipSlice = mip;
		desc.Texture2DArray.ArraySize = 1;
		desc.Texture2DArray.FirstArraySlice = face;
		d3d.device->CreateRenderTargetView((ID3D11Resource*)cube->texture2D, &desc, &cube->rtv);
	}
	ASSERT(d3d.current_framebuffer.count < (u32)lengthOf(d3d.current_framebuffer.render_targets));
	d3d.current_framebuffer.render_targets[d3d.current_framebuffer.count] = cube->rtv;

	ID3D11ShaderResourceView* tmp[16] = {};
	d3d.device_ctx->VSGetShaderResources(0, lengthOf(tmp), tmp);
	for (ID3D11ShaderResourceView*& srv : tmp) {
		if (cube->srv == srv) {
			const u32 idx = u32(&srv - tmp);
			ID3D11ShaderResourceView* empty = nullptr;
			d3d.device_ctx->VSSetShaderResources(idx, 1, &empty);
			d3d.device_ctx->PSSetShaderResources(idx, 1, &empty);
		}
	}

	++d3d.current_framebuffer.count;

	d3d.device_ctx->OMSetRenderTargets(d3d.current_framebuffer.count, d3d.current_framebuffer.render_targets, d3d.current_framebuffer.depth_stencil);
}

// TODO texture might get destroyed while framebuffer has rtv or dsv to it
void setFramebuffer(TextureHandle* attachments, u32 num, TextureHandle ds, u32 flags) {
	ASSERT(num < (u32)lengthOf(d3d.current_framebuffer.render_targets));
	checkThread();

	const bool readonly_ds = flags & (u32)FramebufferFlags::READONLY_DEPTH_STENCIL;
	if (!attachments && !ds) {
		d3d.current_framebuffer = d3d.current_window->framebuffer;
		d3d.device_ctx->OMSetRenderTargets(d3d.current_framebuffer.count, d3d.current_framebuffer.render_targets, d3d.current_framebuffer.depth_stencil);
		return;
	}

	d3d.current_framebuffer.count = num;
	for(u32 i = 0; i < num; ++i) {
		ASSERT(attachments[i]);
		Texture& t = *attachments[i];
		if (t.bound_to_input != 0xffFFffFF && d3d.bound_textures[t.bound_to_input] == &t) {
			ID3D11ShaderResourceView* empty = nullptr;
			d3d.device_ctx->VSSetShaderResources(t.bound_to_input, 1, &empty);
			d3d.device_ctx->PSSetShaderResources(t.bound_to_input, 1, &empty);
			d3d.device_ctx->CSSetShaderResources(t.bound_to_input, 1, &empty);
			d3d.bound_textures[t.bound_to_input] = INVALID_TEXTURE;
			t.bound_to_input = 0xffFFffFF;
		}

		if(!t.rtv) {
			D3D11_RENDER_TARGET_VIEW_DESC desc = {};
			desc.Format = t.dxgi_format;
			desc.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2D;
			desc.Texture2D.MipSlice = 0;
			d3d.device->CreateRenderTargetView((ID3D11Resource*)t.texture2D, &desc, &t.rtv);
		}
		d3d.current_framebuffer.render_targets[i] = t.rtv;
	}

	if (ds) {
		Texture& t = *ds;
		if(readonly_ds && !t.dsv_ro) {
			D3D11_DEPTH_STENCIL_VIEW_DESC desc = {};
			desc.Format = toDSViewFormat(t.dxgi_format);
			desc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
			desc.Texture2D.MipSlice = 0;
			desc.Flags = D3D11_DSV_READ_ONLY_DEPTH;
			d3d.device->CreateDepthStencilView((ID3D11Resource*)t.texture2D, &desc, &t.dsv_ro);
		}
		else if(!t.dsv) {
			D3D11_DEPTH_STENCIL_VIEW_DESC desc = {};
			desc.Format = toDSViewFormat(t.dxgi_format);
			desc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
			desc.Texture2D.MipSlice = 0;
			d3d.device->CreateDepthStencilView((ID3D11Resource*)t.texture2D, &desc, &t.dsv);
		}
		d3d.current_framebuffer.depth_stencil = readonly_ds ? t.dsv_ro : t.dsv;
	}
	else {
		d3d.current_framebuffer.depth_stencil = nullptr;
	}

	d3d.device_ctx->OMSetRenderTargets(d3d.current_framebuffer.count, d3d.current_framebuffer.render_targets, d3d.current_framebuffer.depth_stencil);
}

void clear(u32 flags, const float* color, float depth)
{
	if (flags & (u32)ClearFlags::COLOR) {
		for (u32 i = 0; i < d3d.current_framebuffer.count; ++i) {
			d3d.device_ctx->ClearRenderTargetView(d3d.current_framebuffer.render_targets[i], color);
		}
	}
	u32 ds_flags = 0;
	if (flags & (u32)ClearFlags::DEPTH) {
		ds_flags |= D3D11_CLEAR_DEPTH;
	}
	if (flags & (u32)ClearFlags::STENCIL) {
		ds_flags |= D3D11_CLEAR_STENCIL;
	}
	if (ds_flags && d3d.current_framebuffer.depth_stencil) {
		d3d.device_ctx->ClearDepthStencilView(d3d.current_framebuffer.depth_stencil, ds_flags, depth, 0);
	}
}

void* map(BufferHandle buffer, size_t size)
{
	ASSERT(buffer);
	D3D11_MAP map = D3D11_MAP_WRITE_DISCARD;
	ASSERT(!buffer->mapped_ptr);
	D3D11_MAPPED_SUBRESOURCE msr;
	d3d.device_ctx->Map(buffer->buffer, 0, map, 0, &msr);
	buffer->mapped_ptr = (u8*)msr.pData;
	return buffer->mapped_ptr;
}

void unmap(BufferHandle buffer)
{
	ASSERT(buffer);
	ASSERT(buffer->mapped_ptr);
	d3d.device_ctx->Unmap(buffer->buffer, 0);
	buffer->mapped_ptr = nullptr;
}

bool getMemoryStats(Ref<MemoryStats> stats) { return false; }

void setCurrentWindow(void* window_handle)
{
	checkThread();
	
	if (!window_handle) {
		d3d.current_window = &d3d.windows[0];
		return;
	}

	for (auto& window : d3d.windows) {
		if (window.handle == window_handle) {
			d3d.current_window = &window;
			return;
		}
	}

	for (auto& window : d3d.windows) {
		if (window.handle) continue;

		window.handle = window_handle;
		d3d.current_window = &window;
		RECT rect;
		GetClientRect((HWND)window_handle, &rect);
		window.size = IVec2(rect.right - rect.left, rect.bottom - rect.top);
		d3d.current_window = &window;

		const int width = rect.right - rect.left;
		const int height = rect.bottom - rect.top;

		typedef HRESULT (WINAPI* PFN_CREATE_DXGI_FACTORY)(REFIID _riid, void** _factory);
		static const GUID IID_IDXGIFactory    = { 0x7b7166ec, 0x21c7, 0x44ae, { 0xb2, 0x1a, 0xc9, 0xae, 0x32, 0x1a, 0xe3, 0x69 } };
		PFN_CREATE_DXGI_FACTORY CreateDXGIFactory1 = (PFN_CREATE_DXGI_FACTORY)GetProcAddress(d3d.dxgi_dll, "CreateDXGIFactory1");
			
		::IDXGIFactory5* factory;
		CreateDXGIFactory1(IID_IDXGIFactory, (void**)&factory);

		DXGI_SWAP_CHAIN_DESC desc = {};
		desc.BufferDesc.Width = width;
		desc.BufferDesc.Height = height;
		desc.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
		desc.BufferDesc.RefreshRate.Numerator = 60;
		desc.BufferDesc.RefreshRate.Denominator = 1;
		desc.OutputWindow = (HWND)window_handle;
		desc.Windowed = true;
		desc.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
		desc.BufferCount = 1;
		desc.SampleDesc.Count = 1;
		desc.SampleDesc.Quality = 0;
		desc.Flags = DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH;
		desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
		HRESULT hr = factory->CreateSwapChain(d3d.device, &desc, &window.swapchain);

		if(!SUCCEEDED(hr)) {
			logError("gpu") << "Failed to create swapchain";
			return;
		}

		ID3D11Texture2D* rt;
		hr = window.swapchain->GetBuffer(0, IID_ID3D11Texture2D, (void**)&rt);
		if(!SUCCEEDED(hr)) {
			logError("gpu") << "Failed to get swapchain's buffer";
			return;
		}

		hr = d3d.device->CreateRenderTargetView((ID3D11Resource*)rt, NULL, &window.framebuffer.render_targets[0]);
		rt->Release();
		if(!SUCCEEDED(hr)) {
			logError("gpu") << "Failed to create RTV";
			return;
		}

		window.framebuffer.count = 1;
	
		d3d.current_framebuffer = window.framebuffer;
		factory->Release();
		return;
	}

	logError("gpu") << "Too many windows created.";
	ASSERT(false);
}

u32 swapBuffers()
{
	if(d3d.disjoint_waiting) {
		D3D11_QUERY_DATA_TIMESTAMP_DISJOINT disjoint_query_data;
		const HRESULT res = d3d.device_ctx->GetData(d3d.disjoint_query, &disjoint_query_data, sizeof(disjoint_query_data), 0);
		if (res == S_OK && disjoint_query_data.Disjoint == FALSE) {
			d3d.query_frequency = disjoint_query_data.Frequency;
			d3d.device_ctx->Begin(d3d.disjoint_query);
			d3d.disjoint_waiting = false;
		}
	}
	else {
		d3d.device_ctx->End(d3d.disjoint_query);
		d3d.disjoint_waiting = true;
	}

	for (auto& window : d3d.windows) {
		if (!window.handle) continue;

		window.swapchain->Present(1, 0);

		RECT rect;
		GetClientRect((HWND)window.handle, &rect);

		const IVec2 size(rect.right - rect.left, rect.bottom - rect.top);
		if (size != window.size && size.x != 0) {
			window.size = size;
			bool has_ds = false;
			if (window.framebuffer.depth_stencil) {
				has_ds = true;
				window.framebuffer.depth_stencil->Release();
				window.framebuffer.depth_stencil = nullptr;
			}
			window.framebuffer.render_targets[0]->Release();

			ID3D11Texture2D* rt;
			d3d.device_ctx->OMSetRenderTargets(0, nullptr, 0);
			d3d.device_ctx->ClearState();
			window.swapchain->ResizeBuffers(1, size.x, size.y, DXGI_FORMAT_UNKNOWN, DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH);
			HRESULT hr = window.swapchain->GetBuffer(0, IID_ID3D11Texture2D, (void**)&rt);
			ASSERT(SUCCEEDED(hr));

			hr = d3d.device->CreateRenderTargetView((ID3D11Resource*)rt, NULL, &window.framebuffer.render_targets[0]);
			rt->Release();
			ASSERT(SUCCEEDED(hr));
			window.framebuffer.count = 1;
		
			if (has_ds) {
				D3D11_TEXTURE2D_DESC ds_desc;
				memset(&ds_desc, 0, sizeof(ds_desc));
				ds_desc.Width = size.x;
				ds_desc.Height = size.y;
				ds_desc.MipLevels = 1;
				ds_desc.ArraySize = 1;
				ds_desc.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
				ds_desc.SampleDesc.Count = 1;
				ds_desc.SampleDesc.Quality = 0;
				ds_desc.Usage = D3D11_USAGE_DEFAULT;
				ds_desc.BindFlags = D3D11_BIND_DEPTH_STENCIL;

				ID3D11Texture2D* ds;
				hr = d3d.device->CreateTexture2D(&ds_desc, NULL, &ds);
				ASSERT(SUCCEEDED(hr));

				ds->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen("win_ds"), "win_ds");

				D3D11_DEPTH_STENCIL_VIEW_DESC dsv_desc;
				memset(&dsv_desc, 0, sizeof(dsv_desc));
				dsv_desc.Format = ds_desc.Format;
				dsv_desc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
				dsv_desc.Texture2D.MipSlice = 0;

				hr = d3d.device->CreateDepthStencilView((ID3D11Resource*)ds, &dsv_desc, &window.framebuffer.depth_stencil);
				ASSERT(SUCCEEDED(hr));
				ds->Release();
			}
		}
	}
	d3d.current_framebuffer = d3d.windows[0].framebuffer;
	return 0;
}

void waitFrame(u32 frame) {}

void createBuffer(BufferHandle buffer, u32 flags, size_t size, const void* data)
{
	ASSERT(buffer);
	ASSERT(!buffer->buffer);
	D3D11_BUFFER_DESC desc = {};
	if(flags & (u32)BufferFlags::SHADER_BUFFER) {
		size = ((size + 15) / 16) * 16;
	}

	desc.ByteWidth = (UINT)size;
	buffer->is_constant_buffer = flags & (u32)BufferFlags::UNIFORM_BUFFER;
	if (flags & (u32)BufferFlags::UNIFORM_BUFFER) {
		desc.BindFlags = D3D11_BIND_CONSTANT_BUFFER; 
	}
	else {
		desc.BindFlags = D3D11_BIND_VERTEX_BUFFER | D3D11_BIND_INDEX_BUFFER; 
		if (flags & (u32)BufferFlags::SHADER_BUFFER) {
			desc.BindFlags |= D3D11_BIND_SHADER_RESOURCE;
			desc.MiscFlags = D3D11_RESOURCE_MISC_BUFFER_ALLOW_RAW_VIEWS;
			if (flags & (u32)BufferFlags::COMPUTE_WRITE) {
				desc.BindFlags |= D3D11_BIND_UNORDERED_ACCESS;
				desc.MiscFlags |= D3D11_RESOURCE_MISC_DRAWINDIRECT_ARGS;
			}
		}
	}

	if (flags & (u32)BufferFlags::IMMUTABLE) {
		desc.Usage = D3D11_USAGE_IMMUTABLE;
	}
	else if (flags & (u32)BufferFlags::COMPUTE_WRITE) {
		desc.Usage = D3D11_USAGE_DEFAULT;
	}
	else {
		desc.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
		desc.Usage = D3D11_USAGE_DYNAMIC;
	}
	D3D11_SUBRESOURCE_DATA initial_data = {};
	initial_data.pSysMem = data;
	d3d.device->CreateBuffer(&desc, data ? &initial_data : nullptr, &buffer->buffer);

	if(flags & (u32)BufferFlags::SHADER_BUFFER) {
		D3D11_SHADER_RESOURCE_VIEW_DESC srv_desc = {};
		srv_desc.Format = DXGI_FORMAT_R32_TYPELESS;
		srv_desc.ViewDimension = D3D11_SRV_DIMENSION_BUFFEREX;
		srv_desc.BufferEx.Flags = D3D11_BUFFEREX_SRV_FLAG_RAW;
		srv_desc.BufferEx.FirstElement = 0;
		
		srv_desc.BufferEx.NumElements = UINT(size / 4);

		d3d.device->CreateShaderResourceView(buffer->buffer, &srv_desc, &buffer->srv);

		if (flags & (u32)BufferFlags::COMPUTE_WRITE) {
			D3D11_UNORDERED_ACCESS_VIEW_DESC uav_desc = {};
			uav_desc.Format = DXGI_FORMAT_R32_TYPELESS;
			uav_desc.ViewDimension = D3D11_UAV_DIMENSION_BUFFER;
			uav_desc.Buffer.FirstElement = 0;
			uav_desc.Buffer.NumElements = UINT(size / sizeof(float));
			uav_desc.Buffer.Flags = D3D11_BUFFER_UAV_FLAG_RAW;
			d3d.device->CreateUnorderedAccessView(buffer->buffer, &uav_desc, &buffer->uav);
		}
	}
}

ProgramHandle allocProgramHandle()
{
	Program* p = LUMIX_NEW(*d3d.allocator, Program);
	*p = {};
	return { p };
}

BufferHandle allocBufferHandle()
{
	Buffer* b = LUMIX_NEW(*d3d.allocator, Buffer);
	return { b };
}

TextureHandle allocTextureHandle()
{
	Texture* t = LUMIX_NEW(*d3d.allocator, Texture);
	return { t };
}

void VertexDecl::addAttribute(u8 idx, u8 byte_offset, u8 components_num, AttributeType type, u8 flags) {
	if((int)attributes_count >= lengthOf(attributes)) {
		ASSERT(false);
		return;
	}

	Attribute& attr = attributes[attributes_count];
	attr.components_count = components_num;
	attr.idx = idx;
	attr.flags = flags;
	attr.type = type;
	attr.byte_offset = byte_offset;
	hash = crc32(attributes, sizeof(Attribute) * attributes_count);
	++attributes_count;
}

ID3D11SamplerState* getSampler(u32 flags) {
	const u32 idx = flags & 0b1111;
	if (!d3d.samplers[idx]) {
		D3D11_SAMPLER_DESC sampler_desc = {};
		sampler_desc.Filter = (flags & (u32)TextureFlags::POINT_FILTER) ? D3D11_FILTER_MIN_MAG_MIP_POINT : D3D11_FILTER_MIN_MAG_MIP_LINEAR;
		sampler_desc.AddressU = (flags & (u32)TextureFlags::CLAMP_U) ? D3D11_TEXTURE_ADDRESS_CLAMP : D3D11_TEXTURE_ADDRESS_WRAP;
		sampler_desc.AddressV = (flags & (u32)TextureFlags::CLAMP_V) ? D3D11_TEXTURE_ADDRESS_CLAMP : D3D11_TEXTURE_ADDRESS_WRAP;
		sampler_desc.AddressW = (flags & (u32)TextureFlags::CLAMP_W) ? D3D11_TEXTURE_ADDRESS_CLAMP : D3D11_TEXTURE_ADDRESS_WRAP;
		sampler_desc.MipLODBias = 0.f;
		sampler_desc.ComparisonFunc = D3D11_COMPARISON_ALWAYS;
		sampler_desc.MinLOD = 0.f;
		sampler_desc.MaxLOD = D3D11_FLOAT32_MAX;
		d3d.device->CreateSamplerState(&sampler_desc, &d3d.samplers[idx]);
	}

	return d3d.samplers[idx];
}

bool loadTexture(TextureHandle handle, const void* data, int size, u32 flags, const char* debug_name) { 
	ASSERT(debug_name && debug_name[0]);
	ASSERT(handle);
	checkThread();
	DDS::Header hdr;

	InputMemoryStream blob(data, size);
	blob.read(&hdr, sizeof(hdr));

	if (hdr.dwMagic != DDS::DDS_MAGIC || hdr.dwSize != 124 ||
		!(hdr.dwFlags & DDS::DDSD_PIXELFORMAT) || !(hdr.dwFlags & DDS::DDSD_CAPS))
	{
		logError("renderer") << "Wrong dds format or corrupted dds (" << debug_name << ")";
		return false;
	}

	DDS::LoadInfo* li;
	int layers = 1;

	if (isDXT1(hdr.pixelFormat)) {
		li = &DDS::loadInfoDXT1;
	}
	else if (isDXT3(hdr.pixelFormat)) {
		li = &DDS::loadInfoDXT3;
	}
	else if (isDXT5(hdr.pixelFormat)) {
		li = &DDS::loadInfoDXT5;
	}
	else if (isATI1(hdr.pixelFormat)) {
		li = &DDS::loadInfoATI1;
	}
	else if (isATI2(hdr.pixelFormat)) {
		li = &DDS::loadInfoATI2;
	}
	else if (isBGRA8(hdr.pixelFormat)) {
		li = &DDS::loadInfoBGRA8;
	}
	else if (isBGR8(hdr.pixelFormat)) {
		li = &DDS::loadInfoBGR8;
	}
	else if (isBGR5A1(hdr.pixelFormat)) {
		li = &DDS::loadInfoBGR5A1;
	}
	else if (isBGR565(hdr.pixelFormat)) {
		li = &DDS::loadInfoBGR565;
	}
	else if (isINDEX8(hdr.pixelFormat)) {
		li = &DDS::loadInfoIndex8;
	}
	else if (isDXT10(hdr.pixelFormat)) {
		DDS::DXT10Header dxt10_hdr;
		blob.read(dxt10_hdr);
		li = DDS::getDXT10LoadInfo(hdr, dxt10_hdr);
		layers = dxt10_hdr.array_size;
	}
	else {
		ASSERT(false);
		return false;
	}

	const bool is_cubemap = (hdr.caps2.dwCaps2 & DDS::DDSCAPS2_CUBEMAP) != 0;
	const bool is_srgb = flags & (u32)TextureFlags::SRGB;
	const DXGI_FORMAT internal_format = is_srgb ? li->srgb_format : li->format;
	const u32 mip_count = (hdr.dwFlags & DDS::DDSD_MIPMAPCOUNT) ? hdr.dwMipMapCount : 1;
	Texture& texture = *handle;
	texture.flags = flags;
	texture.w = hdr.dwWidth;
	texture.h = hdr.dwHeight;
	#ifdef LUMIX_DEBUG
		texture.name = debug_name;
	#endif

	D3D11_SUBRESOURCE_DATA* srd = (D3D11_SUBRESOURCE_DATA*)_alloca(sizeof(D3D11_SUBRESOURCE_DATA) * mip_count * layers * (is_cubemap ? 6 : 1));
	u32 srd_idx = 0;
	for(int side = 0; side < (is_cubemap ? 6 : 1); ++side) {
		for (int layer = 0; layer < layers; ++layer) {
			//const GLenum tex_img_target =  is_cubemap ? GL_TEXTURE_CUBE_MAP_POSITIVE_X + side : layers > 1 ? GL_TEXTURE_2D_ARRAY : GL_TEXTURE_2D;

			if (li->compressed) {
				/*if (size != hdr.dwPitchOrLinearSize || (hdr.dwFlags & DDS::DDSD_LINEARSIZE) == 0) {
					CHECK_GL(glDeleteTextures(1, &texture));
					return false;
				}*/
				for (u32 mip = 0; mip < mip_count; ++mip) {
					const u32 width = maximum(1, hdr.dwWidth >> mip);
					const u32 height = maximum(1, hdr.dwHeight >> mip);
					const u32 size = DDS::sizeDXTC(width, height, internal_format);
					srd[srd_idx].pSysMem = (u8*)blob.getData() + blob.getPosition();
					srd[srd_idx].SysMemPitch = ((width + 3) / 4) * DDS::sizeDXTC(1, 1, internal_format);
					srd[srd_idx].SysMemSlicePitch = ((height + 3) / 4) * srd[srd_idx].SysMemPitch;
					blob.skip(size);
					ASSERT(size == srd[srd_idx].SysMemSlicePitch);
					++srd_idx;
				}
			}
			else {
				// TODO
				ASSERT(false);
			}
		}
	}

	if (is_cubemap) {
		D3D11_TEXTURE2D_DESC desc = {};

		desc.Width = maximum(li->block_width, hdr.dwWidth);
		desc.Height = maximum(li->block_width, hdr.dwHeight);
		desc.ArraySize = 6;
		desc.MipLevels = mip_count;
		desc.CPUAccessFlags = 0;
		desc.Format = is_srgb ? li->srgb_format : li->format;
		desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
		desc.MiscFlags = 0;
		desc.Usage = D3D11_USAGE_DEFAULT;
		desc.SampleDesc.Count = 1;
		desc.MiscFlags = D3D11_RESOURCE_MISC_TEXTURECUBE;
		texture.dxgi_format = desc.Format;
		HRESULT hr = d3d.device->CreateTexture2D(&desc, srd, &texture.texture2D);
		ASSERT(SUCCEEDED(hr));

		D3D11_SHADER_RESOURCE_VIEW_DESC srv_desc = {};
		srv_desc.Format = toViewFormat(desc.Format);
		srv_desc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURECUBE;
		srv_desc.TextureCube.MipLevels = mip_count;

		hr = d3d.device->CreateShaderResourceView(texture.texture2D, &srv_desc, &texture.srv);
		ASSERT(SUCCEEDED(hr));
	} else if (layers > 1) {
		D3D11_TEXTURE2D_DESC desc = {};

		desc.Width = maximum(li->block_width, hdr.dwWidth);
		desc.Height = maximum(li->block_width, hdr.dwHeight);
		desc.ArraySize = layers;
		desc.MipLevels = mip_count;
		desc.CPUAccessFlags = 0;
		desc.Format = is_srgb ? li->srgb_format : li->format;
		desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
		desc.MiscFlags = 0;
		desc.Usage = D3D11_USAGE_DEFAULT;
		desc.SampleDesc.Count = 1;
		texture.dxgi_format = desc.Format;
		HRESULT hr = d3d.device->CreateTexture2D(&desc, srd, &texture.texture2D);
		ASSERT(SUCCEEDED(hr));

		D3D11_SHADER_RESOURCE_VIEW_DESC srv_desc = {};
		srv_desc.Format = toViewFormat(desc.Format);
		srv_desc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2DARRAY;
		srv_desc.Texture2DArray.MipLevels = mip_count;
		srv_desc.Texture2DArray.ArraySize = layers;
		srv_desc.Texture2DArray.FirstArraySlice = 0;

		hr = d3d.device->CreateShaderResourceView(texture.texture2D, &srv_desc, &texture.srv);
		ASSERT(SUCCEEDED(hr));
	} else {
		D3D11_TEXTURE2D_DESC desc = {};
		desc.Width = maximum(li->block_width, hdr.dwWidth);
		desc.Height = maximum(li->block_width, hdr.dwHeight);
		desc.ArraySize = layers;
		desc.MipLevels = mip_count;
		desc.CPUAccessFlags = 0;
		desc.Format = is_srgb ? li->srgb_format : li->format;
		desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
		desc.MiscFlags = 0;
		desc.SampleDesc.Count = 1;
		desc.Usage = D3D11_USAGE_DEFAULT;
		texture.dxgi_format = desc.Format;
		HRESULT hr = d3d.device->CreateTexture2D(&desc, srd, &texture.texture2D);
		ASSERT(SUCCEEDED(hr));

		D3D11_SHADER_RESOURCE_VIEW_DESC srv_desc = {};
		srv_desc.Format = toViewFormat(desc.Format);
		srv_desc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
		srv_desc.Texture2D.MipLevels = mip_count;

		hr = d3d.device->CreateShaderResourceView(texture.texture2D, &srv_desc, &texture.srv);
		ASSERT(SUCCEEDED(hr));
	}

	texture.sampler = getSampler(flags);

	return true;
}

bool createTexture(TextureHandle handle, u32 w, u32 h, u32 depth, TextureFormat format, u32 flags, const void* data, const char* debug_name)
{
	ASSERT(handle);
	const bool is_srgb = flags & (u32)TextureFlags::SRGB;
	const bool no_mips = flags & (u32)TextureFlags::NO_MIPS;
	const bool readback = flags & (u32)TextureFlags::READBACK;
	const bool is_3d = flags & (u32)TextureFlags::IS_3D;
	const bool is_cubemap = flags & (u32)TextureFlags::IS_CUBE;
	const bool compute_write = flags & (u32)TextureFlags::COMPUTE_WRITE;

	switch (format) {
		case TextureFormat::R8:
		case TextureFormat::RGBA8:
		case TextureFormat::RGBA32F:
		case TextureFormat::R32F:
		case TextureFormat::RG32F:
		case TextureFormat::SRGB:
		case TextureFormat::SRGBA: break;
		
		case TextureFormat::R16:
		case TextureFormat::RGBA16:
		case TextureFormat::R16F: 
		case TextureFormat::RGBA16F: 
		case TextureFormat::D32: 
		case TextureFormat::D24: 
		case TextureFormat::D24S8: ASSERT(no_mips); break;
		default: ASSERT(false); return false;
	}

	const u32 mip_count = no_mips ? 1 : 1 + log2(maximum(w, h, depth));
	Texture& texture = *handle;
	texture.flags = flags;
	texture.sampler = getSampler(flags);
	texture.w = w;
	texture.h = h;
	#ifdef LUMIX_DEBUG
		texture.name = debug_name;
	#endif

	D3D11_TEXTURE3D_DESC desc_3d = {};
	D3D11_TEXTURE2D_DESC desc_2d = {};
	
	auto fill_desc = [&](auto& desc){
		desc.Width = w;
		desc.Height = h;
		desc.MipLevels = mip_count;
		desc.Usage = D3D11_USAGE_DEFAULT;
		desc.CPUAccessFlags = 0;
		desc.Format = getDXGIFormat(format);
		const bool is_depth_format = isDepthFormat(desc.Format);
		if (is_depth_format) {
			desc.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_DEPTH_STENCIL;
		}
		else {
			desc.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET;
		}
		if (compute_write) {
			desc.BindFlags |= D3D11_BIND_UNORDERED_ACCESS;
		}
		if (readback) {
			desc.Usage = D3D11_USAGE_STAGING;
			desc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
			desc.BindFlags = 0;
		}
		desc.MiscFlags = readback || no_mips || is_depth_format ? 0 : D3D11_RESOURCE_MISC_GENERATE_MIPS;
		texture.dxgi_format = desc.Format;
	};

	u32 bytes_per_pixel ;
	if (is_3d) {
		fill_desc(desc_3d);
		desc_3d.Depth = depth;
		bytes_per_pixel = getSize(desc_3d.Format);
	}
	else {
		fill_desc(desc_2d);
		desc_2d.SampleDesc.Count = 1;
		desc_2d.ArraySize = is_cubemap ? 6 : depth;
		desc_2d.MiscFlags |= is_cubemap ? D3D11_RESOURCE_MISC_TEXTURECUBE : 0;
		bytes_per_pixel = getSize(desc_2d.Format);
	}

	Array<Array<u8>> mips_data(*d3d.allocator);
	mips_data.reserve(mip_count - 1);
	if(data) {
		D3D11_SUBRESOURCE_DATA* srd = (D3D11_SUBRESOURCE_DATA*)_alloca(sizeof(D3D11_SUBRESOURCE_DATA) * mip_count * (is_cubemap ? 6 : depth));
		const u8* ptr = (u8*)data;
		
		// TODO some formats are transformed to different sized dxgi formats
		u32 idx = 0;
		for(u32 layer = 0; layer < (is_cubemap ? 6 : depth); ++layer) {
			srd[idx].pSysMem = ptr;
			srd[idx].SysMemPitch = w * bytes_per_pixel;
			srd[idx].SysMemSlicePitch = h * srd[idx].SysMemPitch;
			++idx;
			u32 prev_mip_w = w;
			u32 prev_mip_h = h;
			const u8* prev_mip_data = ptr;
			ptr += w * h * bytes_per_pixel;
			for (u32 mip = 1; mip < mip_count; ++mip) {
				Array<u8>& mip_data = mips_data.emplace(*d3d.allocator);
				const u32 mip_w = maximum(w >> mip, 1);
				const u32 mip_h = maximum(h >> mip, 1);
				mip_data.resize(bytes_per_pixel * mip_w * mip_h);
				switch(format) {
					case TextureFormat::R8:
						stbir_resize_uint8(prev_mip_data, prev_mip_w, prev_mip_h, 0, mip_data.begin(), maximum(1, prev_mip_w >> 1), maximum(1, prev_mip_h >> 1), 0, 1);
						break;
					case TextureFormat::SRGBA:
					case TextureFormat::RGBA8:
						stbir_resize_uint8(prev_mip_data, prev_mip_w, prev_mip_h, 0, mip_data.begin(), maximum(1, prev_mip_w >> 1), maximum(1, prev_mip_h >> 1), 0, 4);
						break;
					case TextureFormat::SRGB:
						stbir_resize_uint8(prev_mip_data, prev_mip_w, prev_mip_h, 0, mip_data.begin(), maximum(1, prev_mip_w >> 1), maximum(1, prev_mip_h >> 1), 0, 3);
						break;
					case TextureFormat::R32F:
						stbir_resize_float((const float*)prev_mip_data, prev_mip_w, prev_mip_h, 0, (float*)mip_data.begin(), maximum(1, prev_mip_w >> 1), maximum(1, prev_mip_h >> 1), 0, 1);
						break;
					case TextureFormat::RGBA32F:
						stbir_resize_float((const float*)prev_mip_data, prev_mip_w, prev_mip_h, 0, (float*)mip_data.begin(), maximum(1, prev_mip_w >> 1), maximum(1, prev_mip_h >> 1), 0, 4);
						break;
					default: ASSERT(false); return false;
				}
				prev_mip_w = mip_w;
				prev_mip_h = mip_h;
				prev_mip_data = mip_data.begin();
				srd[idx].pSysMem = mip_data.begin();
				srd[idx].SysMemPitch = mip_w * bytes_per_pixel;
				srd[idx].SysMemSlicePitch = mip_h * srd[idx].SysMemPitch;
				++idx;
			}
		}

		if (is_3d) {
			d3d.device->CreateTexture3D(&desc_3d, srd, &texture.texture3D);
		}
		else {
			d3d.device->CreateTexture2D(&desc_2d, srd, &texture.texture2D);
		}
	}
	else {
		if (is_3d) {
			d3d.device->CreateTexture3D(&desc_3d, nullptr, &texture.texture3D);
		}
		else {
			d3d.device->CreateTexture2D(&desc_2d, nullptr, &texture.texture2D);
		}
	}

	if (is_3d) {
		ASSERT(texture.texture3D);
		if(debug_name && debug_name[0]) {
			texture.texture3D->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(debug_name), debug_name);
		}
	}
	else {
		ASSERT(texture.texture2D);
		if(debug_name && debug_name[0]) {
			texture.texture2D->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(debug_name), debug_name);
		}
	}

	if (compute_write) {
		D3D11_UNORDERED_ACCESS_VIEW_DESC uav_desc = {};
		if (is_3d) {
			uav_desc.Format = texture.dxgi_format;
			uav_desc.ViewDimension = D3D11_UAV_DIMENSION_TEXTURE3D;
			uav_desc.Texture3D.MipSlice = 0;
			uav_desc.Texture3D.WSize = -1;
			uav_desc.Texture3D.FirstWSlice = 0;
			d3d.device->CreateUnorderedAccessView(texture.texture3D, &uav_desc, &texture.uav);
		}
		else {
			uav_desc.Format = texture.dxgi_format;
			uav_desc.ViewDimension = D3D11_UAV_DIMENSION_TEXTURE2D;
			uav_desc.Texture2D.MipSlice = 0;
			d3d.device->CreateUnorderedAccessView(texture.texture2D, &uav_desc, &texture.uav);
		}
	}

	if (!readback) {
		D3D11_SHADER_RESOURCE_VIEW_DESC srv_desc = {};
		if (is_3d) {
			srv_desc.Format = toViewFormat(desc_3d.Format);
			srv_desc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE3D;
			srv_desc.Texture3D.MipLevels = mip_count;
			d3d.device->CreateShaderResourceView(texture.texture3D, &srv_desc, &texture.srv);
		}
		else if (is_cubemap) {
			srv_desc.Format = toViewFormat(desc_2d.Format);
			srv_desc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURECUBE;
			srv_desc.TextureCube.MipLevels = mip_count;
			d3d.device->CreateShaderResourceView(texture.texture2D, &srv_desc, &texture.srv);
		}
		else {
			srv_desc.Format = toViewFormat(desc_2d.Format);
			srv_desc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
			srv_desc.Texture2D.MipLevels = mip_count;
			d3d.device->CreateShaderResourceView(texture.texture2D, &srv_desc, &texture.srv);
		}

	}
	return true;
}

void setState(u64 state)
{
	auto iter = d3d.state_cache.find(state);
	if (!iter.isValid()) {
		D3D11_BLEND_DESC blend_desc = {};
		D3D11_RASTERIZER_DESC desc = {};
		D3D11_DEPTH_STENCIL_DESC depthStencilDesc = {};
	
		if (state & u64(StateFlags::CULL_BACK)) {
			desc.CullMode = D3D11_CULL_BACK;
		}
		else if(state & u64(StateFlags::CULL_FRONT)) {
			desc.CullMode = D3D11_CULL_FRONT;
		}
		else {
			desc.CullMode = D3D11_CULL_NONE;
		}

		desc.FrontCounterClockwise = TRUE;
		desc.FillMode =  (state & u64(StateFlags::WIREFRAME)) != 0 ? D3D11_FILL_WIREFRAME : D3D11_FILL_SOLID;
		desc.ScissorEnable = (state & u64(StateFlags::SCISSOR_TEST)) != 0;
		desc.DepthClipEnable = FALSE;

		depthStencilDesc.DepthEnable = (state & u64(StateFlags::DEPTH_TEST)) != 0;
		depthStencilDesc.DepthWriteMask = (state & u64(StateFlags::DEPTH_WRITE)) != 0 ? D3D11_DEPTH_WRITE_MASK_ALL : D3D11_DEPTH_WRITE_MASK_ZERO;
		depthStencilDesc.DepthFunc = (state & u64(StateFlags::DEPTH_TEST)) != 0 ? D3D11_COMPARISON_GREATER_EQUAL : D3D11_COMPARISON_ALWAYS;

		const StencilFuncs func = (StencilFuncs)((state >> 30) & 0xf);
		depthStencilDesc.StencilEnable = func != StencilFuncs::DISABLE; 
		if(depthStencilDesc.StencilEnable) {

			depthStencilDesc.StencilReadMask = u8(state >> 42);
			depthStencilDesc.StencilWriteMask = u8(state >> 22);
			D3D11_COMPARISON_FUNC dx_func;
			switch(func) {
				case StencilFuncs::ALWAYS: dx_func = D3D11_COMPARISON_ALWAYS; break;
				case StencilFuncs::EQUAL: dx_func = D3D11_COMPARISON_EQUAL; break;
				case StencilFuncs::NOT_EQUAL: dx_func = D3D11_COMPARISON_NOT_EQUAL; break;
				default: ASSERT(false); break;
			}
			auto toDXOp = [](StencilOps op) {
				constexpr D3D11_STENCIL_OP table[] = {
					D3D11_STENCIL_OP_KEEP,
					D3D11_STENCIL_OP_ZERO,
					D3D11_STENCIL_OP_REPLACE,
					D3D11_STENCIL_OP_INCR_SAT,
					D3D11_STENCIL_OP_DECR_SAT,
					D3D11_STENCIL_OP_INVERT,
					D3D11_STENCIL_OP_INCR,
					D3D11_STENCIL_OP_DECR
				};
				return table[(int)op];
			};
			const D3D11_STENCIL_OP sfail = toDXOp(StencilOps((state >> 50) & 0xf));
			const D3D11_STENCIL_OP zfail = toDXOp(StencilOps((state >> 54) & 0xf));
			const D3D11_STENCIL_OP zpass = toDXOp(StencilOps((state >> 58) & 0xf));

			depthStencilDesc.FrontFace.StencilFailOp = sfail;
			depthStencilDesc.FrontFace.StencilDepthFailOp = zfail;
			depthStencilDesc.FrontFace.StencilPassOp = zpass;
			depthStencilDesc.FrontFace.StencilFunc = dx_func;

			depthStencilDesc.BackFace.StencilFailOp = sfail;
			depthStencilDesc.BackFace.StencilDepthFailOp = zfail;
			depthStencilDesc.BackFace.StencilPassOp = zpass;
			depthStencilDesc.BackFace.StencilFunc = dx_func;
		}

		u16 blend_bits = u16(state >> 6);

		auto to_dx = [&](BlendFactors factor) -> D3D11_BLEND {
			static const D3D11_BLEND table[] = {
				D3D11_BLEND_ZERO,
				D3D11_BLEND_ONE,
				D3D11_BLEND_SRC_COLOR,
				D3D11_BLEND_INV_SRC_COLOR,
				D3D11_BLEND_SRC_ALPHA,
				D3D11_BLEND_INV_SRC_ALPHA,
				D3D11_BLEND_DEST_COLOR,
				D3D11_BLEND_INV_DEST_COLOR,
				D3D11_BLEND_DEST_ALPHA,
				D3D11_BLEND_INV_DEST_ALPHA,
				D3D11_BLEND_SRC1_COLOR,
				D3D11_BLEND_INV_SRC1_COLOR,
				D3D11_BLEND_SRC1_ALPHA,
				D3D11_BLEND_INV_SRC1_ALPHA,
			};
			ASSERT((u32)factor < lengthOf(table));
			return table[(int)factor];
		};

		for(u32 rt_idx = 0; rt_idx < (u32)lengthOf(blend_desc.RenderTarget); ++rt_idx) {
			if (blend_bits) {
				const BlendFactors src_rgb = (BlendFactors)(blend_bits & 0xf);
				const BlendFactors dst_rgb = (BlendFactors)((blend_bits >> 4) & 0xf);
				const BlendFactors src_a = (BlendFactors)((blend_bits >> 8) & 0xf);
				const BlendFactors dst_a = (BlendFactors)((blend_bits >> 12) & 0xf);
		
				blend_desc.RenderTarget[rt_idx].BlendEnable = true;
				blend_desc.AlphaToCoverageEnable = false;
				blend_desc.RenderTarget[rt_idx].SrcBlend = to_dx(src_rgb);
				blend_desc.RenderTarget[rt_idx].DestBlend = to_dx(dst_rgb);
				blend_desc.RenderTarget[rt_idx].BlendOp = D3D11_BLEND_OP_ADD;
				blend_desc.RenderTarget[rt_idx].SrcBlendAlpha = to_dx(src_a);
				blend_desc.RenderTarget[rt_idx].DestBlendAlpha = to_dx(dst_a);
				blend_desc.RenderTarget[rt_idx].BlendOpAlpha = D3D11_BLEND_OP_ADD;
				blend_desc.RenderTarget[rt_idx].RenderTargetWriteMask = D3D11_COLOR_WRITE_ENABLE_ALL;
			}
			else {
				blend_desc.RenderTarget[rt_idx].BlendEnable = false;
				blend_desc.RenderTarget[rt_idx].SrcBlend = D3D11_BLEND_SRC_ALPHA;
				blend_desc.RenderTarget[rt_idx].DestBlend = D3D11_BLEND_INV_SRC_ALPHA;
				blend_desc.RenderTarget[rt_idx].BlendOp = D3D11_BLEND_OP_ADD;
				blend_desc.RenderTarget[rt_idx].SrcBlendAlpha = D3D11_BLEND_SRC_ALPHA;
				blend_desc.RenderTarget[rt_idx].DestBlendAlpha = D3D11_BLEND_INV_SRC_ALPHA;
				blend_desc.RenderTarget[rt_idx].BlendOpAlpha = D3D11_BLEND_OP_ADD;
				blend_desc.RenderTarget[rt_idx].RenderTargetWriteMask = D3D11_COLOR_WRITE_ENABLE_ALL;
			}
		}

		D3D::State s;
		d3d.device->CreateDepthStencilState(&depthStencilDesc, &s.dss);
		d3d.device->CreateRasterizerState(&desc, &s.rs);
		d3d.device->CreateBlendState(&blend_desc, &s.bs);

		d3d.state_cache.insert(state, s);
		iter = d3d.state_cache.find(state);
	}

	const u8 stencil_ref = u8(state >> 34);
	const D3D::State& s = iter.value();

	float blend_factor[4] = {};
	d3d.device_ctx->OMSetDepthStencilState(s.dss, stencil_ref);
	d3d.device_ctx->RSSetState(s.rs);
	d3d.device_ctx->OMSetBlendState(s.bs, blend_factor, 0xffFFffFF);
}

void viewport(u32 x, u32 y, u32 w, u32 h)
{
	D3D11_VIEWPORT vp;
	memset(&vp, 0, sizeof(D3D11_VIEWPORT));
	vp.Width =  (float)w;
	vp.Height = (float)h;
	vp.MinDepth = 0.0f;
	vp.MaxDepth = 1.0f;
	vp.TopLeftX = (float)x;
	vp.TopLeftY = (float)y;
	d3d.device_ctx->RSSetViewports(1, &vp);
}

void useProgram(ProgramHandle program)
{
	d3d.current_program = program;
	if (program) {
		d3d.device_ctx->VSSetShader(program->vs, nullptr, 0);
		d3d.device_ctx->PSSetShader(program->ps, nullptr, 0);
		d3d.device_ctx->GSSetShader(program->gs, nullptr, 0);
		d3d.device_ctx->CSSetShader(program->cs, nullptr, 0);
		d3d.device_ctx->IASetInputLayout(program->il);
	}
	else {
		d3d.device_ctx->VSSetShader(nullptr, nullptr, 0);
		d3d.device_ctx->PSSetShader(nullptr, nullptr, 0);
		d3d.device_ctx->GSSetShader(nullptr, nullptr, 0);
		d3d.device_ctx->CSSetShader(nullptr, nullptr, 0);
	}
}

void scissor(u32 x, u32 y, u32 w, u32 h) {
	RECT r;
	r.left = x;
	r.top = y;
	r.right = x + w;
	r.bottom = y + h;
	d3d.device_ctx->RSSetScissorRects(1, &r);
}

void drawTriangles(u32 bytes_offset, u32 indices_count, DataType index_type) {
	DXGI_FORMAT dxgi_index_type;
	switch(index_type) {
		case DataType::U32: dxgi_index_type = DXGI_FORMAT_R32_UINT; break;
		case DataType::U16: dxgi_index_type = DXGI_FORMAT_R16_UINT; break;
	}

	ASSERT(d3d.current_index_buffer);
	ID3D11Buffer* b = d3d.current_index_buffer->buffer;
	d3d.device_ctx->IASetIndexBuffer(b, dxgi_index_type, bytes_offset);
	d3d.device_ctx->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
	d3d.device_ctx->DrawIndexed(indices_count, 0, 0);
}

void drawArrays(u32 offset, u32 count, PrimitiveType type)
{
	D3D11_PRIMITIVE_TOPOLOGY topology;
	switch(type) {
		case PrimitiveType::LINES: topology = D3D_PRIMITIVE_TOPOLOGY_LINELIST; break;
		case PrimitiveType::POINTS: topology = D3D_PRIMITIVE_TOPOLOGY_POINTLIST; break;
		case PrimitiveType::TRIANGLES: topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST; break;
		case PrimitiveType::TRIANGLE_STRIP: topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP; break;
		default: ASSERT(false); return;
	}
	d3d.device_ctx->IASetPrimitiveTopology(topology);
	d3d.device_ctx->Draw(count, offset);
}

bool isOriginBottomLeft() { return false; }

TextureInfo getTextureInfo(const void* data)
{
	TextureInfo info;

	const DDS::Header* hdr = (const DDS::Header*)data;
	info.width = hdr->dwWidth;
	info.height = hdr->dwHeight;
	info.is_cubemap = (hdr->caps2.dwCaps2 & DDS::DDSCAPS2_CUBEMAP) != 0;
	info.mips = (hdr->dwFlags & DDS::DDSD_MIPMAPCOUNT) ? hdr->dwMipMapCount : 1;
	info.depth = (hdr->dwFlags & DDS::DDSD_DEPTH) ? hdr->dwDepth : 1;
	
	if (isDXT10(hdr->pixelFormat)) {
		const DDS::DXT10Header* hdr_dxt10 = (const DDS::DXT10Header*)((const u8*)data + sizeof(DDS::Header));
		info.layers = hdr_dxt10->array_size;
	}
	else {
		info.layers = 1;
	}
	
	return info;
}

void destroy(BufferHandle buffer) {
	checkThread();
	LUMIX_DELETE(*d3d.allocator, buffer);
}

void bindShaderBuffer(BufferHandle buffer, u32 binding_point, u32 flags)
{
	if(buffer) {
		Buffer& b = *buffer;
		if (flags & (u32)BindShaderBufferFlags::OUTPUT && b.uav) {
			d3d.device_ctx->CSSetUnorderedAccessViews(binding_point, 1, &b.uav, nullptr);
			b.bound_to_output = binding_point;
			d3d.bound_uavs[binding_point] = buffer;
		}
		else {
			if (b.bound_to_output) {
				if (d3d.bound_uavs[b.bound_to_output] == buffer) {
					ID3D11UnorderedAccessView* uav = nullptr;
					d3d.device_ctx->CSSetUnorderedAccessViews(b.bound_to_output, 1, &uav, nullptr);
					d3d.bound_uavs[b.bound_to_output] = 0;
				}
				b.bound_to_output = 0;
			}

			d3d.device_ctx->CSSetShaderResources(binding_point, 1, &b.srv);
			d3d.device_ctx->VSSetShaderResources(binding_point, 1, &b.srv);
			d3d.device_ctx->PSSetShaderResources(binding_point, 1, &b.srv);
		}
	}
	else {
		ID3D11ShaderResourceView* srv = nullptr;
		ID3D11UnorderedAccessView* uav = nullptr;
		d3d.device_ctx->VSSetShaderResources(binding_point, 1, &srv);
		d3d.device_ctx->PSSetShaderResources(binding_point, 1, &srv);
		d3d.device_ctx->CSSetShaderResources(binding_point, 1, &srv);
		d3d.device_ctx->CSSetUnorderedAccessViews(binding_point, 1, &uav, nullptr);
	}
}

void bindUniformBuffer(u32 index, BufferHandle buffer, size_t offset, size_t size) {
	ASSERT(buffer);
	ID3D11Buffer* b = buffer->buffer;
	ASSERT(offset % 16 == 0);
	const UINT first = (UINT)offset / 16;
	const UINT num = ((UINT)size + 255) / 256 * 16;
	d3d.device_ctx->VSSetConstantBuffers1(index, 1, &b, &first, &num);
	d3d.device_ctx->PSSetConstantBuffers1(index, 1, &b, &first, &num);
	d3d.device_ctx->CSSetConstantBuffers1(index, 1, &b, &first, &num);
}

void drawIndirect(DataType index_type) {
	DXGI_FORMAT dxgi_index_type;
	switch(index_type) {
		case DataType::U32: dxgi_index_type = DXGI_FORMAT_R32_UINT; break;
		case DataType::U16: dxgi_index_type = DXGI_FORMAT_R16_UINT; break;
	}

	ASSERT(d3d.current_index_buffer);
	ASSERT(d3d.current_indirect_buffer);
	
	ID3D11Buffer* index_b = d3d.current_index_buffer->buffer;
	ID3D11Buffer* indirect_b = d3d.current_indirect_buffer->buffer;
	d3d.device_ctx->IASetIndexBuffer(index_b, dxgi_index_type, 0);
	d3d.device_ctx->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
	d3d.device_ctx->DrawIndexedInstancedIndirect(indirect_b, 0);
}

void bindIndirectBuffer(BufferHandle handle) {
	d3d.current_indirect_buffer = handle;
}

void bindIndexBuffer(BufferHandle handle) {
	d3d.current_index_buffer = handle;
}

void dispatch(u32 num_groups_x, u32 num_groups_y, u32 num_groups_z) {
	d3d.device_ctx->Dispatch(num_groups_x, num_groups_y, num_groups_z);
}

void bindVertexBuffer(u32 binding_idx, BufferHandle buffer, u32 buffer_offset, u32 stride_offset) {
	if(buffer) {
		ID3D11Buffer* b = buffer->buffer;
		d3d.device_ctx->IASetVertexBuffers(binding_idx, 1, &b, &stride_offset, &buffer_offset);
	}
	else {
		ID3D11Buffer* tmp = nullptr;
		UINT tmp2 = 0;
		d3d.device_ctx->IASetVertexBuffers(binding_idx, 1, &tmp, &tmp2, &tmp2);
	}
}

void bindImageTexture(TextureHandle handle, u32 unit) {
	
	if (handle) {
		Texture& texture = *handle;
		texture.bound_to_output = unit;
		d3d.device_ctx->CSSetUnorderedAccessViews(unit, 1, &texture.uav, nullptr);
		d3d.bound_image_textures[unit] = handle;
	}
	else {
		ID3D11UnorderedAccessView* uav = nullptr;
		d3d.device_ctx->CSSetUnorderedAccessViews(unit, 1, &uav, nullptr);
		d3d.bound_image_textures[unit] = INVALID_TEXTURE;
	}
}

void bindTextures(const TextureHandle* handles, u32 offset, u32 count) {
	ID3D11ShaderResourceView* views[16];
	ID3D11SamplerState* samplers[16];
	for (u32 i = 0; i < count; ++i) {
		d3d.bound_textures[offset + i] = handles[i];
		if (handles[i]) {
			Texture& texture = *handles[i];
			views[i] = texture.srv;
			samplers[i] = texture.sampler;
			texture.bound_to_input = i;
			if (texture.bound_to_output != 0xffFFffFF && d3d.bound_image_textures[texture.bound_to_output] == handles[i]) {
				ID3D11UnorderedAccessView* uav = nullptr;
				d3d.device_ctx->CSSetUnorderedAccessViews(texture.bound_to_output, 1, &uav, nullptr);
			}
		}
		else {
			views[i] = nullptr;
			samplers[i] = nullptr;
		}
	}
	d3d.device_ctx->VSSetShaderResources(offset, count, views);
	d3d.device_ctx->PSSetShaderResources(offset, count, views);
	d3d.device_ctx->CSSetShaderResources(offset, count, views);
	d3d.device_ctx->PSSetSamplers(offset, count, samplers);
	d3d.device_ctx->VSSetSamplers(offset, count, samplers);
	d3d.device_ctx->CSSetSamplers(offset, count, samplers);
}

void drawTrianglesInstanced(u32 indices_count, u32 instances_count, DataType index_type) {
	ASSERT(d3d.current_index_buffer);
	DXGI_FORMAT dxgi_index_type;
	switch(index_type) {
		case DataType::U32: dxgi_index_type = DXGI_FORMAT_R32_UINT; break;
		case DataType::U16: dxgi_index_type = DXGI_FORMAT_R16_UINT; break;
	}

	ID3D11Buffer* b = d3d.current_index_buffer->buffer;
	d3d.device_ctx->IASetIndexBuffer(b, dxgi_index_type, 0);
	d3d.device_ctx->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
	d3d.device_ctx->DrawIndexedInstanced(indices_count, instances_count, 0, 0, 0);
}

void drawElements(u32 offset, u32 count, PrimitiveType primitive_type, DataType index_type) {
	ASSERT(d3d.current_index_buffer);
	D3D11_PRIMITIVE_TOPOLOGY pt;
	switch (primitive_type) {
		case PrimitiveType::TRIANGLES: pt = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST; break;
		case PrimitiveType::TRIANGLE_STRIP: pt = D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP; break;
		case PrimitiveType::LINES: pt = D3D_PRIMITIVE_TOPOLOGY_LINELIST; break;
		case PrimitiveType::POINTS: pt = D3D_PRIMITIVE_TOPOLOGY_POINTLIST; break;
		default: ASSERT(0); break;
	} 

	DXGI_FORMAT dxgi_index_type;
	u32 offset_shift = 0;
	switch(index_type) {
		case DataType::U32: dxgi_index_type = DXGI_FORMAT_R32_UINT; offset_shift = 2; break;
		case DataType::U16: dxgi_index_type = DXGI_FORMAT_R16_UINT; offset_shift = 1; break;
	}

	ASSERT((offset & (offset_shift - 1)) == 0);
	ID3D11Buffer* b = d3d.current_index_buffer->buffer;
	d3d.device_ctx->IASetIndexBuffer(b, dxgi_index_type, 0);
	d3d.device_ctx->IASetPrimitiveTopology(pt);
	d3d.device_ctx->DrawIndexed(count, offset >> offset_shift, 0);
}

void copy(BufferHandle dst, BufferHandle src, u32 dst_offset, u32 size) {
	ASSERT(dst);
	ASSERT(src);
	ASSERT(!dst->mapped_ptr);
	ASSERT(!src->mapped_ptr);
	D3D11_BOX src_box = {};
	src_box.right = size;
	src_box.bottom = 1;
	src_box.back = 1;
	d3d.device_ctx->CopySubresourceRegion(dst->buffer, 0, dst_offset, 0, 0, src->buffer, 0, &src_box);
}

void update(BufferHandle buffer, const void* data, size_t size) {
	checkThread();
	ASSERT(buffer);
	ASSERT(!buffer->mapped_ptr);

	if (buffer->uav) {
		d3d.device_ctx->UpdateSubresource1(buffer->buffer, 0, nullptr, data, (UINT)size, (UINT)size, D3D11_COPY_DISCARD);
	}
	else {
		D3D11_MAPPED_SUBRESOURCE msr;
		d3d.device_ctx->Map(buffer->buffer, 0, D3D11_MAP_WRITE_DISCARD, 0, &msr);
		memcpy((u8*)msr.pData, data, size);
		d3d.device_ctx->Unmap(buffer->buffer, 0);
	}
}

bool createProgram(ProgramHandle program, const VertexDecl& decl, const char** srcs, const ShaderType* types, u32 num, const char** prefixes, u32 prefixes_count, const char* name)
{
	ASSERT(program);
	#ifdef LUMIX_DEBUG
		program->name = name;
	#endif

	ShaderCompiler::Input args { decl, Span(srcs, num), Span(types, num), Span(prefixes, prefixes_count) };

	if (!d3d.shader_compiler.compile(d3d.device, args, name, Ref(*program))) return false;
	return true;
}

} // ns gpu

} // ns Lumix
