#include "renderer/gpu/gpu.h"
#include "../external/include/glslang/Public/ShaderLang.h"
#include "../external/include/SPIRV/GlslangToSpv.h"
#include "../external/include/spirv_cross/spirv_hlsl.hpp"
#include "engine/array.h"
#include "engine/crc32.h"
#include "engine/hash_map.h"
#include "engine/log.h"
#include "engine/math.h"
#include "engine/sync.h"
#include "engine/stream.h"
#include <Windows.h>
#include <d3d11_1.h>
#include <dxgi.h>
#include <dxgi1_6.h>
#include <d3dcompiler.h>
#include <cassert>
#include <malloc.h>
#include "renderer/gpu/dds.h"
#include "renderer/gpu/renderdoc_app.h"
#include "stb/stb_image_resize.h"

#pragma comment(lib, "d3dcompiler.lib")
#pragma comment(lib, "glslang.lib")
#pragma comment(lib, "OSDependent.lib")
#pragma comment(lib, "OGLCompiler.lib")
#pragma comment(lib, "HLSL.lib")
#pragma comment(lib, "SPIRV.lib")
#pragma comment(lib, "spirv-cross-core.lib")
#pragma comment(lib, "spirv-cross-cpp.lib")
#pragma comment(lib, "spirv-cross-glsl.lib")
#pragma comment(lib, "spirv-cross-hlsl.lib")

static const GUID WKPDID_D3DDebugObjectName     = { 0x429b8c22, 0x9188, 0x4b0c, { 0x87, 0x42, 0xac, 0xb0, 0xbf, 0x85, 0xc2, 0x00 } };
static const GUID IID_ID3D11Texture2D           = { 0x6f15aaf2, 0xd208, 0x4e89, { 0x9a, 0xb4, 0x48, 0x95, 0x35, 0xd3, 0x4f, 0x9c } };
static const GUID IID_ID3D11Device1             = { 0xa04bfb29, 0x08ef, 0x43d6, { 0xa4, 0x9c, 0xa9, 0xbd, 0xbd, 0xcb, 0xe6, 0x86 } };
static const GUID IID_ID3D11Device2             = { 0x9d06dffa, 0xd1e5, 0x4d07, { 0x83, 0xa8, 0x1b, 0xb1, 0x23, 0xf2, 0xf8, 0x41 } };
static const GUID IID_ID3D11Device3             = { 0xa05c8c37, 0xd2c6, 0x4732, { 0xb3, 0xa0, 0x9c, 0xe0, 0xb0, 0xdc, 0x9a, 0xe6 } };
static const GUID IID_ID3D11InfoQueue           = { 0x6543dbb6, 0x1b48, 0x42f5, { 0xab, 0x82, 0xe9, 0x7e, 0xc7, 0x43, 0x26, 0xf6 } };
static const GUID IID_IDXGIDeviceRenderDoc      = { 0xa7aa6116, 0x9c8d, 0x4bba, { 0x90, 0x83, 0xb4, 0xd8, 0x16, 0xb7, 0x1b, 0x78 } };
static const GUID IID_ID3DUserDefinedAnnotation = { 0xb2daad8b, 0x03d4, 0x4dbf, { 0x95, 0xeb, 0x32, 0xab, 0x4b, 0x63, 0xd0, 0xab } };

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

template <typename T, u32 MAX_COUNT>
struct Pool
{
	void init()
	{
		values = (T*)mem;
		for (int i = 0; i < MAX_COUNT; ++i) {
			new (&values[i]) int(i + 1);
		}
		new (&values[MAX_COUNT - 1]) int(-1);
		first_free = 0;
	}

	template <typename... Args>
	int alloc(Args&&... args)
	{
		if(first_free == -1) return -1;

		++count;
		const int id = first_free;
		first_free = *((int*)&values[id]);
		new (&values[id]) T(args...);
		return id;
	}

	void dealloc(u32 idx)
	{
		--count;
		values[idx].~T();
		*((int*)&values[idx]) = first_free;
		first_free = idx;
	}

	alignas(T) u8 mem[sizeof(T) * MAX_COUNT];
	T* values;
	int first_free;
	u32 count = 0;

	T& operator[](int idx) { return values[idx]; }
	bool isFull() const { return first_free == -1; }

	static constexpr u32 CAPACITY = MAX_COUNT;
};

struct Program {
	ID3D11VertexShader* vs = nullptr;
	ID3D11PixelShader* ps = nullptr;
	ID3D11GeometryShader* gs = nullptr;
	ID3D11ComputeShader* cs = nullptr;
	ID3D11InputLayout* il = nullptr;
};

struct Buffer {
	ID3D11Buffer* buffer = nullptr;
	ID3D11ShaderResourceView* srv = nullptr;
	ID3D11UnorderedAccessView* uav = nullptr;
	u8* mapped_ptr = nullptr;
	bool is_constant_buffer = false;
};

struct Texture {
	union {
		ID3D11Texture2D* texture2D;
		ID3D11Texture3D* texture3D;
	};
	union {
		ID3D11RenderTargetView* rtv;
		struct {
			ID3D11DepthStencilView* dsv;
			ID3D11DepthStencilView* dsv_ro;
		};
	};
	ID3D11UnorderedAccessView* uav = nullptr;
	u32 rtv_face = 0;
	u32 rtv_mip = 0;
	u32 flags;
	u32 w;
	u32 h;
	ID3D11ShaderResourceView* srv;
	ID3D11SamplerState* sampler;
	DXGI_FORMAT dxgi_format;
	u32 bound_to_output = 0xffFFffFF;
};

struct Query {
	ID3D11Query* query;
};

struct InputLayout {
	ID3D11InputLayout* layout;
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
		: state_cache(state_allocator) 
	{}

	DWORD thread;
	RENDERDOC_API_1_0_2* rdoc_api;
	IAllocator* allocator = nullptr;
	ID3D11DeviceContext1* device_ctx = nullptr;
	TextureHandle bound_image_textures[16];
	ID3D11Device* device = nullptr;
	ID3DUserDefinedAnnotation* annotation = nullptr;
	ID3D11Query* disjoint_query = nullptr;
	bool disjoint_waiting = false;
	u64 query_frequency = 1;

	BufferHandle current_index_buffer = INVALID_BUFFER;
	BufferHandle current_indirect_buffer = INVALID_BUFFER;
	Mutex handle_mutex;
	Pool<Query, 2048> queries;
	Pool<Program, 256> programs;
	Pool<Buffer, 8192> buffers;
	Pool<Texture, 4096> textures;
	Window windows[64];
	Window* current_window = windows;
	ID3D11SamplerState* samplers[2*2*2*2];

	FrameBuffer current_framebuffer;

	DefaultAllocator state_allocator;
	HashMap<u64, State> state_cache;
	HMODULE d3d_dll;
	HMODULE dxgi_dll;
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
	if (!lib) return;
	pRENDERDOC_GetAPI RENDERDOC_GetAPI = (pRENDERDOC_GetAPI)GetProcAddress(lib, "RENDERDOC_GetAPI");
	if (RENDERDOC_GetAPI) {
		RENDERDOC_GetAPI(eRENDERDOC_API_Version_1_0_2, (void **)&d3d.rdoc_api);
		d3d.rdoc_api->MaskOverlayBits(~RENDERDOC_OverlayBits::eRENDERDOC_Overlay_Enabled, 0);
	}
	/**/
	//FreeLibrary(lib);
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
	MutexGuard lock(d3d.handle_mutex);
	const int idx = d3d.queries.alloc();
	d3d.queries[idx] = {};
	D3D11_QUERY_DESC desc = {};
	desc.Query = D3D11_QUERY_TIMESTAMP;
	d3d.device->CreateQuery(&desc, &d3d.queries[idx].query);
	ASSERT(d3d.queries[idx].query);
	return { (u32)idx }; 
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
	
	Program& p = d3d.programs[program.value];
	if (p.gs) p.gs->Release();
	if (p.ps) p.ps->Release();
	if (p.vs) p.vs->Release();
	if (p.cs) p.cs->Release();
	if (p.il) p.il->Release();

	MutexGuard lock(d3d.handle_mutex);
	d3d.programs.dealloc(program.value);
}

void destroy(TextureHandle texture) {
	checkThread();
	Texture& t = d3d.textures[texture.value];
	if (t.srv) t.srv->Release();
	
	if (isDepthFormat(t.dxgi_format)) {
		if (t.dsv_ro) t.dsv_ro->Release();
		if (t.dsv) t.dsv->Release();
	}
	else {
		if (t.rtv) t.rtv->Release();
	}

	if (t.uav) t.uav->Release();
	if (t.texture2D) t.texture2D->Release();

	MutexGuard lock(d3d.handle_mutex);
	d3d.textures.dealloc(texture.value);
}

void destroy(QueryHandle query) {
	checkThread();

	d3d.queries[query.value].query->Release();

	MutexGuard lock(d3d.handle_mutex);
	d3d.queries.dealloc(query.value);
}

void drawTriangleStripArraysInstanced(u32 indices_count, u32 instances_count) {
	d3d.device_ctx->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
	d3d.device_ctx->DrawInstanced(indices_count, instances_count, 0, 0);
}

void createTextureView(TextureHandle view_handle, TextureHandle texture_handle) {
	Texture& texture = d3d.textures[texture_handle.value];
	Texture& view = d3d.textures[view_handle.value];
	view.dxgi_format = texture.dxgi_format;
	view.sampler = texture.sampler;
	D3D11_SHADER_RESOURCE_VIEW_DESC srv_desc = {};
	texture.srv->GetDesc(&srv_desc);
	if (srv_desc.ViewDimension != D3D_SRV_DIMENSION_TEXTURE2D) {
		srv_desc.ViewDimension = D3D_SRV_DIMENSION_TEXTURE2D;
	}

	d3d.device->CreateShaderResourceView(texture.texture2D, &srv_desc, &view.srv);
}

void generateMipmaps(TextureHandle handle){
	Texture& t = d3d.textures[handle.value];
	d3d.device_ctx->GenerateMips(t.srv);
}

void update(TextureHandle texture_handle, u32 mip, u32 face, u32 x, u32 y, u32 w, u32 h, TextureFormat format, void* buf) {
	Texture& texture = d3d.textures[texture_handle.value];
	ASSERT(texture.dxgi_format == getDXGIFormat(format));
	const bool no_mips = texture.flags & (u32)TextureFlags::NO_MIPS;
	const u32 mip_count = no_mips ? 1 : 1 + log2(maximum(texture.w, texture.h));
	const UINT subres = D3D11CalcSubresource(mip, face, mip_count);
	const u32 bytes_per_pixel = getSize(texture.dxgi_format);
	const UINT row_pitch = w * bytes_per_pixel;
	const UINT depth_pitch = row_pitch * h;
	D3D11_BOX box;
	box.left = x;
	box.top = y;
	box.right = x + w;
	box.bottom = y + h;
	box.front = 0;
	box.back = 1;
	
	d3d.device_ctx->UpdateSubresource(texture.texture2D, subres, &box, buf, row_pitch, depth_pitch);
}

void copy(TextureHandle dst_handle, TextureHandle src_handle, u32 dst_x, u32 dst_y) {
	Texture& dst = d3d.textures[dst_handle.value];
	Texture& src = d3d.textures[src_handle.value];
	const bool no_mips = src.flags & (u32)TextureFlags::NO_MIPS;
	const u32 src_mip_count = no_mips ? 1 : 1 + log2(maximum(src.w, src.h));
	const u32 dst_mip_count = no_mips ? 1 : 1 + log2(maximum(dst.w, dst.h));

	u32 mip = 0;
	while ((src.w >> mip) != 0 || (src.h >> mip) != 0) {
		const u32 w = maximum(src.w >> mip, 1);
		const u32 h = maximum(src.h >> mip, 1);

		if (src.flags & (u32)TextureFlags::IS_CUBE) {
			for (u32 face = 0; face < 6; ++face) {
				const UINT src_subres = D3D11CalcSubresource(mip, face, src_mip_count);
				const UINT dst_subres = D3D11CalcSubresource(mip, face, dst_mip_count);
				d3d.device_ctx->CopySubresourceRegion(dst.texture2D, dst_subres, dst_x, dst_y, 0, src.texture2D, src_subres, nullptr);
			}
		}
		else {
			const UINT src_subres = D3D11CalcSubresource(mip, 0, src_mip_count);
			const UINT dst_subres = D3D11CalcSubresource(mip, 0, dst_mip_count);
			d3d.device_ctx->CopySubresourceRegion(dst.texture2D, dst_subres, dst_x, dst_y, 0, src.texture2D, src_subres, nullptr);
		}
		++mip;
		if (src.flags & (u32)TextureFlags::NO_MIPS) break;
		if (dst.flags & (u32)TextureFlags::NO_MIPS) break;
	}
}

void readTexture(TextureHandle handle, u32 mip, Span<u8> buf) {
	Texture& texture = d3d.textures[handle.value];

	D3D11_MAPPED_SUBRESOURCE data;
	
	const u32 faces = (texture.flags & (u32)TextureFlags::IS_CUBE) ? 6 : 1;
	const bool no_mips = texture.flags & (u32)TextureFlags::NO_MIPS;
	const u32 mip_count = no_mips ? 1 : 1 + log2(maximum(texture.w, texture.h));
	u8* ptr = buf.begin();
	
	for (u32 face = 0; face < faces; ++face) {
		const UINT subres = D3D11CalcSubresource(mip, face, mip_count);
		d3d.device_ctx->Map(texture.texture2D, subres, D3D11_MAP_READ, 0, &data);
		ASSERT(data.DepthPitch == buf.length() / faces);
		memcpy(ptr, data.pData, data.DepthPitch);
		ptr += data.DepthPitch;
		d3d.device_ctx->Unmap(texture.texture2D, subres);
	}
}

void queryTimestamp(QueryHandle query) {
	checkThread();
	d3d.device_ctx->End(d3d.queries[query.value].query);
}

u64 getQueryFrequency() { return d3d.query_frequency; }

u64 getQueryResult(QueryHandle query) {
	checkThread();
	ID3D11Query* q = d3d.queries[query.value].query;
	u64 time;
	const HRESULT res = d3d.device_ctx->GetData(q, &time, sizeof(time), 0);
	ASSERT(res == S_OK);
	return time;
}

bool isQueryReady(QueryHandle query) { 
	checkThread();
	ID3D11Query* q = d3d.queries[query.value].query;
	const HRESULT res = d3d.device_ctx->GetData(q, nullptr, 0, 0);
	return res == S_OK;
}

void preinit(IAllocator& allocator)
{
	try_load_renderdoc();
	d3d.allocator = &allocator;
	d3d.textures.init();
	d3d.buffers.init();
	d3d.programs.init();
	d3d.queries.init();
}

void shutdown() {
	ShFinalize();

	ASSERT(d3d.buffers.count == 0);
	ASSERT(d3d.programs.count == 0);
	ASSERT(d3d.queries.count == 0);
	ASSERT(d3d.textures.count == 0);

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

	ctx->QueryInterface(__uuidof(ID3D11DeviceContext1), (void**)&d3d.device_ctx);

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

	D3D11_DEPTH_STENCIL_VIEW_DESC dsv_desc;
	memset(&dsv_desc, 0, sizeof(dsv_desc));
	dsv_desc.Format = ds_desc.Format;
	dsv_desc.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;

	hr = d3d.device->CreateDepthStencilView((ID3D11Resource*)ds, &dsv_desc, &d3d.windows[0].framebuffer.depth_stencil);
	if(!SUCCEEDED(hr)) return false;

	d3d.current_framebuffer = d3d.windows[0].framebuffer;

	d3d.device_ctx->QueryInterface(IID_ID3DUserDefinedAnnotation, (void**)&d3d.annotation);

	if(debug) {
		ID3D11Debug* d3d_debug = nullptr;
		hr = d3d.device->QueryInterface(__uuidof(ID3D11Debug), (void**)&d3d_debug);
		if (SUCCEEDED(hr)) {
			ID3D11InfoQueue* info_queue;
			hr = d3d_debug->QueryInterface(__uuidof(ID3D11InfoQueue), (void**)&info_queue);
			if (SUCCEEDED(hr)) {
				info_queue->SetBreakOnSeverity(D3D11_MESSAGE_SEVERITY_ERROR, true);
				info_queue->SetBreakOnSeverity(D3D11_MESSAGE_SEVERITY_CORRUPTION, true);
				//info_queue->SetBreakOnSeverity(D3D11_MESSAGE_SEVERITY_WARNING, true);
				info_queue->Release();
			}
			d3d_debug->Release();
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

	d3d.initialized = true;
	return true;
}

void pushDebugGroup(const char* msg)
{
	WCHAR tmp[128];
	toWChar(tmp, msg);
	d3d.annotation->BeginEvent(tmp);
}

void popDebugGroup()
{
	d3d.annotation->EndEvent();
}

void setFramebufferCube(TextureHandle cube, u32 face, u32 mip)
{
	d3d.current_framebuffer.count = 0;
	d3d.current_framebuffer.depth_stencil = nullptr;
	Texture& t = d3d.textures[cube.value];
	if (t.rtv && (t.rtv_face != face || t.rtv_mip) != mip) {
		t.rtv->Release();
		t.rtv = nullptr;
	}
	if(!t.rtv) {
		D3D11_RENDER_TARGET_VIEW_DESC desc = {};
		desc.Format = t.dxgi_format;
		desc.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2DARRAY;
		desc.Texture2DArray.MipSlice = mip;
		desc.Texture2DArray.ArraySize = 1;
		desc.Texture2DArray.FirstArraySlice = face;
		d3d.device->CreateRenderTargetView((ID3D11Resource*)t.texture2D, &desc, &t.rtv);
	}
	ASSERT(d3d.current_framebuffer.count < (u32)lengthOf(d3d.current_framebuffer.render_targets));
	d3d.current_framebuffer.render_targets[d3d.current_framebuffer.count] = t.rtv;

	ID3D11ShaderResourceView* tmp[16] = {};
	d3d.device_ctx->VSGetShaderResources(0, lengthOf(tmp), tmp);
	for (ID3D11ShaderResourceView*& srv : tmp) {
		if (t.srv == srv) {
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
void setFramebuffer(TextureHandle* attachments, u32 num, u32 flags) {
	checkThread();
	const bool readonly_ds = flags & (u32)FramebufferFlags::READONLY_DEPTH_STENCIL;
	if (!attachments) {
		d3d.current_framebuffer = d3d.current_window->framebuffer;
		d3d.device_ctx->OMSetRenderTargets(d3d.current_framebuffer.count, d3d.current_framebuffer.render_targets, d3d.current_framebuffer.depth_stencil);
		return;
	}
	d3d.current_framebuffer.count = 0;
	d3d.current_framebuffer.depth_stencil = nullptr;
	for(u32 i = 0; i < num; ++i) {
		Texture& t = d3d.textures[attachments[i].value];
		if (isDepthFormat(t.dxgi_format)) {
			ASSERT(!d3d.current_framebuffer.depth_stencil);
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
			if(!t.rtv) {
				D3D11_RENDER_TARGET_VIEW_DESC desc = {};
				desc.Format = t.dxgi_format;
				desc.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2D;
				desc.Texture2D.MipSlice = 0;
				d3d.device->CreateRenderTargetView((ID3D11Resource*)t.texture2D, &desc, &t.rtv);
			}
			ASSERT(d3d.current_framebuffer.count < (u32)lengthOf(d3d.current_framebuffer.render_targets));
			d3d.current_framebuffer.render_targets[d3d.current_framebuffer.count] = t.rtv;
			++d3d.current_framebuffer.count;
		}
	}
	
	ID3D11ShaderResourceView* tmp[16] = {};
	d3d.device_ctx->VSGetShaderResources(0, lengthOf(tmp), tmp);
	for (ID3D11ShaderResourceView*& srv : tmp) {
		for (u32 i = 0; i < num; ++i) {
			Texture& t = d3d.textures[attachments[i].value];
			if (t.srv == srv) {
				ID3D11ShaderResourceView* empty = nullptr;
				const u32 idx = u32(&srv - tmp);
				d3d.device_ctx->VSSetShaderResources(idx, 1, &empty);
				d3d.device_ctx->PSSetShaderResources(idx, 1, &empty);
				break;
			}
		}
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

void* map(BufferHandle handle, size_t size)
{
	Buffer& buffer = d3d.buffers[handle.value];
	D3D11_MAP map = D3D11_MAP_WRITE_DISCARD;
	ASSERT(!buffer.mapped_ptr);
	D3D11_MAPPED_SUBRESOURCE msr;
	d3d.device_ctx->Map(buffer.buffer, 0, map, 0, &msr);
	buffer.mapped_ptr = (u8*)msr.pData;
	return buffer.mapped_ptr;
}

void unmap(BufferHandle handle)
{
	Buffer& buffer = d3d.buffers[handle.value];
	ASSERT(buffer.mapped_ptr);
	d3d.device_ctx->Unmap(buffer.buffer, 0);
	buffer.mapped_ptr = nullptr;
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

void swapBuffers()
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
}

void createBuffer(BufferHandle handle, u32 flags, size_t size, const void* data)
{
	Buffer& buffer = d3d.buffers[handle.value];
	ASSERT(!buffer.buffer);
	D3D11_BUFFER_DESC desc = {};
	if(flags & (u32)BufferFlags::SHADER_BUFFER) {
		size = ((size + 15) / 16) * 16;
	}

	desc.ByteWidth = (UINT)size;
	buffer.is_constant_buffer = flags & (u32)BufferFlags::UNIFORM_BUFFER;
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
	d3d.device->CreateBuffer(&desc, data ? &initial_data : nullptr, &buffer.buffer);

	if(flags & (u32)BufferFlags::SHADER_BUFFER) {
		D3D11_SHADER_RESOURCE_VIEW_DESC srv_desc = {};
		srv_desc.Format = DXGI_FORMAT_R32_TYPELESS;
		srv_desc.ViewDimension = D3D11_SRV_DIMENSION_BUFFEREX;
		srv_desc.BufferEx.Flags = D3D11_BUFFEREX_SRV_FLAG_RAW;
		srv_desc.BufferEx.FirstElement = 0;
		
		srv_desc.BufferEx.NumElements = UINT(size / 4);

		d3d.device->CreateShaderResourceView(buffer.buffer, &srv_desc, &buffer.srv);

		if (flags & (u32)BufferFlags::COMPUTE_WRITE) {
			D3D11_UNORDERED_ACCESS_VIEW_DESC uav_desc = {};
			uav_desc.Format = DXGI_FORMAT_R32_TYPELESS;
			uav_desc.ViewDimension = D3D11_UAV_DIMENSION_BUFFER;
			uav_desc.Buffer.FirstElement = 0;
			uav_desc.Buffer.NumElements = UINT(size / 16);
			uav_desc.Buffer.Flags = D3D11_BUFFER_UAV_FLAG_RAW;
			d3d.device->CreateUnorderedAccessView(buffer.buffer, &uav_desc, &buffer.uav);
		}
	}
}

ProgramHandle allocProgramHandle()
{
	MutexGuard lock(d3d.handle_mutex);

	if(d3d.programs.isFull()) {
		logError("Renderer") << "Not enough free program slots.";
		return INVALID_PROGRAM;
	}
	const int id = d3d.programs.alloc();
	Program& p = d3d.programs[id];
	p = {};
	return { (u32)id };
}

BufferHandle allocBufferHandle()
{
	MutexGuard lock(d3d.handle_mutex);

	if(d3d.buffers.isFull()) {
		logError("Renderer") << "Not enough free buffer slots.";
		return INVALID_BUFFER;
	}
	const int id = d3d.buffers.alloc();
	Buffer& t = d3d.buffers[id];
	t = {};
	return { (u32)id };
}

TextureHandle allocTextureHandle()
{
	MutexGuard lock(d3d.handle_mutex);

	if(d3d.textures.isFull()) {
		logError("Renderer") << "Not enough free texture slots.";
		return INVALID_TEXTURE;
	}
	const int id = d3d.textures.alloc();
	Texture& t = d3d.textures[id];
	t = {};
	return { (u32)id };
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
	Texture& texture = d3d.textures[handle.value];
	texture.flags = flags;
	texture.w = hdr.dwWidth;
	texture.h = hdr.dwHeight;
	
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
	Texture& texture = d3d.textures[handle.value];
	texture.flags = flags;
	texture.sampler = getSampler(flags);
	texture.w = w;
	texture.h = h;

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
	ASSERT(texture.texture2D);

	if(debug_name && debug_name[0]) {
		texture.texture2D->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(debug_name), debug_name);
	}

	if (compute_write) {
		D3D11_UNORDERED_ACCESS_VIEW_DESC uav_desc = {};
		uav_desc.Format = texture.dxgi_format;
		uav_desc.ViewDimension = D3D11_UAV_DIMENSION_TEXTURE2D;
		uav_desc.Texture2D.MipSlice = 0;
		d3d.device->CreateUnorderedAccessView(texture.texture2D, &uav_desc, &texture.uav);
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
			depthStencilDesc.StencilWriteMask = u8(state >> 42);
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
				D3D11_BLEND_INV_DEST_ALPHA
			};
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

void useProgram(ProgramHandle handle)
{
	if(handle.isValid()) {
		Program& program = d3d.programs[handle.value];
		d3d.device_ctx->VSSetShader(program.vs, nullptr, 0);
		d3d.device_ctx->PSSetShader(program.ps, nullptr, 0);
		d3d.device_ctx->GSSetShader(program.gs, nullptr, 0);
		d3d.device_ctx->CSSetShader(program.cs, nullptr, 0);
		d3d.device_ctx->IASetInputLayout(program.il);
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

void drawTriangles(u32 indices_count, DataType index_type) {
	DXGI_FORMAT dxgi_index_type;
	switch(index_type) {
		case DataType::U32: dxgi_index_type = DXGI_FORMAT_R32_UINT; break;
		case DataType::U16: dxgi_index_type = DXGI_FORMAT_R16_UINT; break;
	}

	ID3D11Buffer* b = d3d.buffers[d3d.current_index_buffer.value].buffer;
	d3d.device_ctx->IASetIndexBuffer(b, dxgi_index_type, 0);
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
	
	Buffer& t = d3d.buffers[buffer.value];
	t.buffer->Release();
	if (t.srv) t.srv->Release();
	if (t.uav) t.uav->Release();

	MutexGuard lock(d3d.handle_mutex);
	d3d.buffers.dealloc(buffer.value);
}

void bindShaderBuffer(BufferHandle buffer, u32 binding_point, u32 flags)
{
	if(buffer.isValid()) {
		Buffer& b = d3d.buffers[buffer.value];
		ASSERT(b.srv);
		if (b.uav) {
			if (flags & (u32)BindShaderBufferFlags::OUTPUT) {
				d3d.device_ctx->CSSetUnorderedAccessViews(binding_point, 1, &b.uav, nullptr);
			}
			else {
				d3d.device_ctx->CSSetShaderResources(binding_point, 1, &b.srv);
			}
		}
		else {
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
	ID3D11Buffer* b = d3d.buffers[buffer.value].buffer;
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

	ID3D11Buffer* index_b = d3d.buffers[d3d.current_index_buffer.value].buffer;
	ID3D11Buffer* indirect_b = d3d.buffers[d3d.current_indirect_buffer.value].buffer;
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
	if(buffer.isValid()) {
		ID3D11Buffer* b = d3d.buffers[buffer.value].buffer;
		d3d.device_ctx->IASetVertexBuffers(binding_idx, 1, &b, &stride_offset, &buffer_offset);
	}
	else {
		ID3D11Buffer* tmp = nullptr;
		UINT tmp2 = 0;
		d3d.device_ctx->IASetVertexBuffers(binding_idx, 1, &tmp, &tmp2, &tmp2);
	}
}

void bindImageTexture(TextureHandle handle, u32 unit) {
	
	if (handle.isValid()) {
		Texture& texture = d3d.textures[handle.value];
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
		if (handles[i].isValid()) {
			const Texture& texture = d3d.textures[handles[i].value];
			views[i] = texture.srv;
			samplers[i] = texture.sampler;
			if (texture.bound_to_output != 0xffFFffFF && d3d.bound_image_textures[texture.bound_to_output].value == handles[i].value) {
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
	DXGI_FORMAT dxgi_index_type;
	switch(index_type) {
		case DataType::U32: dxgi_index_type = DXGI_FORMAT_R32_UINT; break;
		case DataType::U16: dxgi_index_type = DXGI_FORMAT_R16_UINT; break;
	}

	ID3D11Buffer* b = d3d.buffers[d3d.current_index_buffer.value].buffer;
	d3d.device_ctx->IASetIndexBuffer(b, dxgi_index_type, 0);
	d3d.device_ctx->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
	d3d.device_ctx->DrawIndexedInstanced(indices_count, instances_count, 0, 0, 0);
}

void drawElements(u32 offset, u32 count, PrimitiveType primitive_type, DataType index_type) {
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
	ID3D11Buffer* b = d3d.buffers[d3d.current_index_buffer.value].buffer;
	d3d.device_ctx->IASetIndexBuffer(b, dxgi_index_type, 0);
	d3d.device_ctx->IASetPrimitiveTopology(pt);
	d3d.device_ctx->DrawIndexed(count, offset >> offset_shift, 0);
}

void copy(BufferHandle dst, BufferHandle src, u32 dst_offset, u32 size) {
	const Buffer& bdst = d3d.buffers[dst.value];
	const Buffer& bsrc = d3d.buffers[src.value];
	ASSERT(!bdst.mapped_ptr);
	ASSERT(!bsrc.mapped_ptr);
	D3D11_BOX src_box = {};
	src_box.right = size;
	src_box.bottom = 1;
	src_box.back = 1;
	d3d.device_ctx->CopySubresourceRegion(bdst.buffer, 0, dst_offset, 0, 0, bsrc.buffer, 0, &src_box);
}

void update(BufferHandle buffer, const void* data, size_t size) {
	checkThread();
	
	const Buffer& b = d3d.buffers[buffer.value];
	ASSERT(!b.mapped_ptr);
	if (b.uav) {
		d3d.device_ctx->UpdateSubresource1(b.buffer, 0, nullptr, data, (UINT)size, (UINT)size, D3D11_COPY_DISCARD);
	}
	else {
		D3D11_MAPPED_SUBRESOURCE msr;
		d3d.device_ctx->Map(b.buffer, 0, D3D11_MAP_WRITE_DISCARD, 0, &msr);
		memcpy((u8*)msr.pData, data, size);
		d3d.device_ctx->Unmap(b.buffer, 0);
	}
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

static const TBuiltInResource DefaultTBuiltInResource = {
	/* .MaxLights = */ 32,
	/* .MaxClipPlanes = */ 6,
	/* .MaxTextureUnits = */ 32,
	/* .MaxTextureCoords = */ 32,
	/* .MaxVertexAttribs = */ 64,
	/* .MaxVertexUniformComponents = */ 4096,
	/* .MaxVaryingFloats = */ 64,
	/* .MaxVertexTextureImageUnits = */ 32,
	/* .MaxCombinedTextureImageUnits = */ 80,
	/* .MaxTextureImageUnits = */ 32,
	/* .MaxFragmentUniformComponents = */ 4096,
	/* .MaxDrawBuffers = */ 32,
	/* .MaxVertexUniformVectors = */ 128,
	/* .MaxVaryingVectors = */ 8,
	/* .MaxFragmentUniformVectors = */ 16,
	/* .MaxVertexOutputVectors = */ 16,
	/* .MaxFragmentInputVectors = */ 15,
	/* .MinProgramTexelOffset = */ -8,
	/* .MaxProgramTexelOffset = */ 7,
	/* .MaxClipDistances = */ 8,
	/* .MaxComputeWorkGroupCountX = */ 65535,
	/* .MaxComputeWorkGroupCountY = */ 65535,
	/* .MaxComputeWorkGroupCountZ = */ 65535,
	/* .MaxComputeWorkGroupSizeX = */ 1024,
	/* .MaxComputeWorkGroupSizeY = */ 1024,
	/* .MaxComputeWorkGroupSizeZ = */ 64,
	/* .MaxComputeUniformComponents = */ 1024,
	/* .MaxComputeTextureImageUnits = */ 16,
	/* .MaxComputeImageUniforms = */ 8,
	/* .MaxComputeAtomicCounters = */ 8,
	/* .MaxComputeAtomicCounterBuffers = */ 1,
	/* .MaxVaryingComponents = */ 60,
	/* .MaxVertexOutputComponents = */ 64,
	/* .MaxGeometryInputComponents = */ 64,
	/* .MaxGeometryOutputComponents = */ 128,
	/* .MaxFragmentInputComponents = */ 128,
	/* .MaxImageUnits = */ 8,
	/* .MaxCombinedImageUnitsAndFragmentOutputs = */ 8,
	/* .MaxCombinedShaderOutputResources = */ 8,
	/* .MaxImageSamples = */ 0,
	/* .MaxVertexImageUniforms = */ 0,
	/* .MaxTessControlImageUniforms = */ 0,
	/* .MaxTessEvaluationImageUniforms = */ 0,
	/* .MaxGeometryImageUniforms = */ 0,
	/* .MaxFragmentImageUniforms = */ 8,
	/* .MaxCombinedImageUniforms = */ 8,
	/* .MaxGeometryTextureImageUnits = */ 16,
	/* .MaxGeometryOutputVertices = */ 256,
	/* .MaxGeometryTotalOutputComponents = */ 1024,
	/* .MaxGeometryUniformComponents = */ 1024,
	/* .MaxGeometryVaryingComponents = */ 64,
	/* .MaxTessControlInputComponents = */ 128,
	/* .MaxTessControlOutputComponents = */ 128,
	/* .MaxTessControlTextureImageUnits = */ 16,
	/* .MaxTessControlUniformComponents = */ 1024,
	/* .MaxTessControlTotalOutputComponents = */ 4096,
	/* .MaxTessEvaluationInputComponents = */ 128,
	/* .MaxTessEvaluationOutputComponents = */ 128,
	/* .MaxTessEvaluationTextureImageUnits = */ 16,
	/* .MaxTessEvaluationUniformComponents = */ 1024,
	/* .MaxTessPatchComponents = */ 120,
	/* .MaxPatchVertices = */ 32,
	/* .MaxTessGenLevel = */ 64,
	/* .MaxViewports = */ 16,
	/* .MaxVertexAtomicCounters = */ 0,
	/* .MaxTessControlAtomicCounters = */ 0,
	/* .MaxTessEvaluationAtomicCounters = */ 0,
	/* .MaxGeometryAtomicCounters = */ 0,
	/* .MaxFragmentAtomicCounters = */ 8,
	/* .MaxCombinedAtomicCounters = */ 8,
	/* .MaxAtomicCounterBindings = */ 1,
	/* .MaxVertexAtomicCounterBuffers = */ 0,
	/* .MaxTessControlAtomicCounterBuffers = */ 0,
	/* .MaxTessEvaluationAtomicCounterBuffers = */ 0,
	/* .MaxGeometryAtomicCounterBuffers = */ 0,
	/* .MaxFragmentAtomicCounterBuffers = */ 1,
	/* .MaxCombinedAtomicCounterBuffers = */ 1,
	/* .MaxAtomicCounterBufferSize = */ 16384,
	/* .MaxTransformFeedbackBuffers = */ 4,
	/* .MaxTransformFeedbackInterleavedComponents = */ 64,
	/* .MaxCullDistances = */ 8,
	/* .MaxCombinedClipAndCullDistances = */ 8,
	/* .MaxSamples = */ 4,
	/* .maxMeshOutputVerticesNV = */ 256,
	/* .maxMeshOutputPrimitivesNV = */ 512,
	/* .maxMeshWorkGroupSizeX_NV = */ 32,
	/* .maxMeshWorkGroupSizeY_NV = */ 1,
	/* .maxMeshWorkGroupSizeZ_NV = */ 1,
	/* .maxTaskWorkGroupSizeX_NV = */ 32,
	/* .maxTaskWorkGroupSizeY_NV = */ 1,
	/* .maxTaskWorkGroupSizeZ_NV = */ 1,
	/* .maxMeshViewCountNV = */ 4,

	/* .limits = */ {
		/* .nonInductiveForLoops = */ 1,
		/* .whileLoops = */ 1,
		/* .doWhileLoops = */ 1,
		/* .generalUniformIndexing = */ 1,
		/* .generalAttributeMatrixVectorIndexing = */ 1,
		/* .generalVaryingIndexing = */ 1,
		/* .generalSamplerIndexing = */ 1,
		/* .generalVariableIndexing = */ 1,
		/* .generalConstantMatrixVectorIndexing = */ 1,
	}
};

static bool glsl2hlsl(const char** srcs, u32 count, ShaderType type, const char* shader_name, Ref<std::string> out) {
	glslang::TProgram p;
	EShLanguage lang = EShLangVertex;
	switch(type) {
		case ShaderType::COMPUTE: lang = EShLangCompute; break;
		case ShaderType::FRAGMENT: lang = EShLangFragment; break;
		case ShaderType::VERTEX: lang = EShLangVertex; break;
		case ShaderType::GEOMETRY: lang = EShLangGeometry; break;
		default: ASSERT(false); break;
	}

	glslang::TShader shader(lang);
	shader.setStrings(srcs, count);
	shader.setEnvInput(glslang::EShSourceGlsl, lang, glslang::EShClientOpenGL, 430);
	shader.setEnvClient(glslang::EShClientOpenGL, glslang::EShTargetClientVersion::EShTargetOpenGL_450);
	shader.setEnvTarget(glslang::EShTargetSpv, glslang::EShTargetLanguageVersion::EShTargetSpv_1_4);
	auto res2 = shader.parse(&DefaultTBuiltInResource, 430, false, EShMsgDefault);
	const char* log = shader.getInfoLog();
	if(!res2) {
		logError("Renderer") << shader_name << ": " << log;
	}
	p.addShader(&shader);
	auto res = p.link(EShMsgDefault);
	if(res2 && res) {
		auto im = p.getIntermediate(lang);
		std::vector<unsigned int> spirv;
		spv::SpvBuildLogger logger;
		glslang::SpvOptions spvOptions;
		spvOptions.generateDebugInfo = true;
		spvOptions.disableOptimizer = true;
		spvOptions.optimizeSize = false;
		spvOptions.disassemble = false;
		spvOptions.validate = true;
		glslang::GlslangToSpv(*im, spirv, &logger, &spvOptions);

		spirv_cross::CompilerHLSL hlsl(spirv);
		spirv_cross::CompilerHLSL::Options options;
		options.shader_model = 50;
		hlsl.set_hlsl_options(options);
		const spirv_cross::VariableID num_workgroups_builtin_id = hlsl.remap_num_workgroups_builtin();
		if (num_workgroups_builtin_id != spirv_cross::VariableID(0)) {
			logError("Renderer") << shader_name << ": there's no hlsl equivalent to gl_NumWorkGroups, use user-provided uniforms instead.";
			return false;
		}
		out = hlsl.compile();
		return true;
	}
	return false;
}

bool createProgram(ProgramHandle handle, const VertexDecl& decl, const char** srcs, const ShaderType* types, u32 num, const char** prefixes, u32 prefixes_count, const char* name)
{
	Program& program = d3d.programs[handle.value];
	program = {};
	void* vs_bytecode = nullptr;
	size_t vs_bytecode_len = 0;
	
	static const char* attr_defines[] = {
		"#define _HAS_ATTR0\n",
		"#define _HAS_ATTR1\n",
		"#define _HAS_ATTR2\n",
		"#define _HAS_ATTR3\n",
		"#define _HAS_ATTR4\n",
		"#define _HAS_ATTR5\n",
		"#define _HAS_ATTR6\n",
		"#define _HAS_ATTR7\n",
		"#define _HAS_ATTR8\n",
		"#define _HAS_ATTR9\n",
		"#define _HAS_ATTR10\n",
		"#define _HAS_ATTR11\n",
		"#define _HAS_ATTR12\n"
	};

	const char* tmp[128];
	auto filter_srcs = [&](ShaderType type) -> u32 {
		switch (type) {
			case ShaderType::COMPUTE: tmp[0] = "#define LUMIX_COMPUTE_SHADER\n"; break;
			case ShaderType::GEOMETRY: tmp[0] = "#define LUMIX_GEOMETRY_SHADER\n"; break;
			case ShaderType::FRAGMENT: tmp[0] = "#define LUMIX_FRAGMENT_SHADER\n"; break;
			case ShaderType::VERTEX: tmp[0] = "#define LUMIX_VERTEX_SHADER\n"; break;
			default: ASSERT(false); return 0;
		}
		for(u32 i = 0; i < prefixes_count; ++i) {
			tmp[i + 1] = prefixes[i];
		}
		for (u32 i = 0; i < decl.attributes_count; ++i) {
			tmp[i + 1 + prefixes_count] = attr_defines[decl.attributes[i].idx]; 
		}

		u32 sc = 0;
		for(u32 i = 0; i < num; ++i) {
			if(types[i] != type) continue;
			tmp[prefixes_count + decl.attributes_count + sc + 1] = srcs[i];
			++sc;
		}
		return sc ? sc + prefixes_count + decl.attributes_count + 1 : 0;
	};
	
	auto compile = [&](const char* src, ShaderType type){
		// TODO cleanup
		ID3DBlob* output = NULL;
		ID3DBlob* errors = NULL;

		HRESULT hr = D3DCompile(src
			, strlen(src) + 1
			, name
			, NULL
			, NULL
			, "main"
			, type == ShaderType::VERTEX ? "vs_5_0" : (type == ShaderType::COMPUTE ? "cs_5_0" : "ps_5_0")
			, D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR | D3DCOMPILE_DEBUG
			, 0
			, &output
			, &errors);
		if (errors) {
			if (SUCCEEDED(hr)) {
				logInfo("gpu") << (LPCSTR)errors->GetBufferPointer();
			}
			else {
				logError("gpu") << (LPCSTR)errors->GetBufferPointer();
			}
			errors->Release();
			if (FAILED(hr)) return false;
		}
		ASSERT(output);

		void* ptr = output->GetBufferPointer();
		size_t len = output->GetBufferSize();

		switch(type) {
			case ShaderType::VERTEX:
				// TODO errors
				vs_bytecode = ptr;
				vs_bytecode_len = len;
				hr = d3d.device->CreateVertexShader(ptr, len, nullptr, &program.vs);
				break;
			case ShaderType::FRAGMENT: hr = d3d.device->CreatePixelShader(ptr, len, nullptr, &program.ps); break;
			case ShaderType::GEOMETRY: hr = d3d.device->CreateGeometryShader(ptr, len, nullptr, &program.gs); break;
			case ShaderType::COMPUTE: hr = d3d.device->CreateComputeShader(ptr, len, nullptr, &program.cs); break;
			default: ASSERT(false); break;
		}
		return SUCCEEDED(hr);
	};

	auto compile_stage = [&](ShaderType type){
		const u32 c = filter_srcs(type);
		if (c == 0) return true;
		if (c > (u32)prefixes_count + decl.attributes_count) {
			std::string hlsl;
			if (!glsl2hlsl(tmp, c, type, name, Ref(hlsl))) {
				return false;
			}
			return compile(hlsl.c_str(), type);
		}
		return false;
	};

	bool compiled = compile_stage(ShaderType::VERTEX);
	compiled = compiled && compile_stage(ShaderType::FRAGMENT);
	compiled = compiled && compile_stage(ShaderType::COMPUTE);
	compiled = compiled && compile_stage(ShaderType::GEOMETRY);
	if(!compiled) return false;

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

	if (vs_bytecode && decl.attributes_count > 0) {
		d3d.device->CreateInputLayout(descs, decl.attributes_count, vs_bytecode, vs_bytecode_len, &program.il);
	}
	else {
		program.il = nullptr;
	}

	if (name && name[0]) {
		if(program.vs) program.vs->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(name), name);
		if(program.ps) program.ps->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(name), name);
		if(program.gs) program.gs->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(name), name);
		if(program.cs) program.cs->SetPrivateData(WKPDID_D3DDebugObjectName, (UINT)strlen(name), name);
	}

	return true;    
}

} // ns gpu

} // ns Lumix
