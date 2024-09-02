#pragma once

#include "core/allocator.h"
#include "core/hash.h"
#include "core/hash_map.h"
#include "core/os.h"
#include "core/profiler.h"
#include "core/tag_allocator.h"
#include <d3dcompiler.h>

#pragma comment(lib, "d3dcompiler.lib")

namespace Lumix::gpu {

static const TBuiltInResource DefaultTBuiltInResource = {/* .MaxLights = */ 32,
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

	/* .limits = */
	{
		/* .nonInductiveForLoops = */ 1,
		/* .whileLoops = */ 1,
		/* .doWhileLoops = */ 1,
		/* .generalUniformIndexing = */ 1,
		/* .generalAttributeMatrixVectorIndexing = */ 1,
		/* .generalVaryingIndexing = */ 1,
		/* .generalSamplerIndexing = */ 1,
		/* .generalVariableIndexing = */ 1,
		/* .generalConstantMatrixVectorIndexing = */ 1,
	}};

struct ShaderCompiler {
	struct Input {
		const VertexDecl& decl;
		Span<const char*> srcs;
		Span<const ShaderType> types;
		Span<const char*> prefixes;
	};
	ShaderCompiler(IAllocator& allocator)
		: m_allocator(allocator, "shader compiler")
		, m_cache(m_allocator) {}

	static u32 filter(const Input& input, ShaderType type, const char* (&out)[128])
	{
		ASSERT(input.srcs.length() == input.types.length());
		out[0] = getTypeDefine(type);
		out[1] = "#define LUMIX_DX_SHADER\n";
		out[2] = R"#(
			#define TextureHandle int
			#define TextureCubeArrayHandle int

			Texture2D<float4> bindless_textures[] : register(t0, space1);
			TextureCubeArray bindless_cube_arrays[] : register(t0, space2);
			Texture2DArray bindless_2D_arrays[] : register(t0, space3);
			TextureCube bindless_cubemaps[] : register(t0, space4);
			RWTexture2D<float4> bindless_rw_textures[] : register(u0, space0);
			RWByteAddressBuffer bindless_rw_buffers[] : register(u0, space1);

			SamplerState LinearSamplerClamp : register(s0);
			SamplerState LinearSampler : register(s1);

			#define sampleCubeBindlessLod(sampler, index, uv, lod) bindless_cubemaps[index].Sample((sampler), (uv), (lod))
			#define sampleCubeBindless(sampler, index, uv) bindless_cubemaps[index].Sample((sampler), (uv))
			#define sampleBindless(sampler, index, uv) bindless_textures[index].Sample((sampler), (uv))
			#define sampleBindlessLod(sampler, index, uv, lod) bindless_textures[index].SampleLevel((sampler), (uv), (lod))
			#define sampleBindlessOffset(sampler, index, uv, offset) bindless_textures[index].Sample((sampler), (uv), (offset))
			#define sampleBindlessLodOffset(sampler, index, uv, lod, offset) bindless_textures[index].SampleLevel((sampler), (uv), (lod), (offset))
			#define sampleCubeArrayBindlessLod(sampler, index, uv, lod) bindless_cube_arrays[index].SampleLevel((sampler), (uv), (lod))
		)#";
		for (u32 i = 0; i < input.decl.attributes_count; ++i) {
			out[i + 3] = getAttrDefine(i); 
		}
		for(u32 i = 0; i < input.prefixes.length(); ++i) {
			out[i + 3 + input.decl.attributes_count] = input.prefixes[i];
		}

		u32 sc = 0;
		for(u32 i = 0; i < input.srcs.length(); ++i) {
			if(input.types[i] != type) continue;
			out[input.prefixes.length() + input.decl.attributes_count + sc + 3] = input.srcs[i];
			++sc;
		}
		return sc ? sc + input.prefixes.length() + input.decl.attributes_count + 3 : 0;
	};

	static StableHash32 computeHash(const char** srcs, u32 count) {
		RollingStableHasher hasher;
		hasher.begin();
		for (u32 i = 0; i < count; ++i) {
			hasher.update(srcs[i], (u32)strlen(srcs[i]));
		}
		return hasher.end();
	}

	ID3DBlob* compile(StableHash32 hash, const char* src, ShaderType type, const char* name, u32 readonly_bitset, u32 used_bitset) {
		ID3DBlob* output = NULL;
		ID3DBlob* errors = NULL;
		HRESULT hr = D3DCompile(src,
			strlen(src) + 1,
			name,
			NULL,
			NULL,
			"main",
			type == ShaderType::VERTEX ? "vs_5_1" : (type == ShaderType::COMPUTE ? "cs_5_1" : "ps_5_1"),
			D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR | D3DCOMPILE_ENABLE_UNBOUNDED_DESCRIPTOR_TABLES | D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION,
			0,
			&output,
			&errors);
		if (errors) {
			if (SUCCEEDED(hr)) {
				logInfo("gpu: ", (LPCSTR)errors->GetBufferPointer());
			} else {
				logError(name, ": ", (LPCSTR)errors->GetBufferPointer());
			}
			errors->Release();
			if (FAILED(hr)) return nullptr;
		}
		ASSERT(output);
		if (m_use_cache) {
			CachedShader cached(m_allocator);
			cached.data.write(output->GetBufferPointer(), output->GetBufferSize());
			cached.readonly_bitset = readonly_bitset;
			cached.used_srvs_bitset = used_bitset;
			m_cache.insert(hash, static_cast<CachedShader&&>(cached));
		}
		return output;
	};

	bool m_use_cache = true;

	void save(const char* filename) {
		os::OutputFile file;
		if (file.open(filename)) {
			u32 version = 0;
			bool success = file.write(&version, sizeof(version));
			for (auto iter = m_cache.begin(), end = m_cache.end(); iter != end; ++iter) {
				const StableHash32 hash = iter.key();
				const CachedShader& s = iter.value();
				const u32 size = (u32)s.data.size();
				success = file.write(&hash, sizeof(hash)) && success;
				success = file.write(&size, sizeof(size)) && success;
				success = file.write(s.data.data(), size) && success;
				success = file.write(&s.readonly_bitset, sizeof(s.readonly_bitset)) && success;
				success = file.write(&s.used_srvs_bitset, sizeof(s.used_srvs_bitset)) && success;
			}
			if (!success) {
				logError("Could not write ", filename);
			}
			file.close();
		}
	}

	void load(const char* filename) {
		PROFILE_FUNCTION();
		os::InputFile file;
		if (file.open(filename)) {
			u32 version;
			if (!file.read(&version, sizeof(version))) {
				logError("Could not read ", filename);
			}
			ASSERT(version == 0);
			StableHash32 hash;
			while (file.read(&hash, sizeof(hash))) {
				u32 size;
				if (file.read(&size, sizeof(size))) {
					CachedShader value(m_allocator);
					value.data.resize(size);
					if (!file.read(value.data.getMutableData(), size)) break;
					if (!file.read(&value.readonly_bitset, sizeof(value.readonly_bitset))) break;
					if (!file.read(&value.used_srvs_bitset, sizeof(value.used_srvs_bitset))) break;
					m_cache.insert(hash, value);
				} else {
					break;
				}
			}
			file.close();
		}
	}

	static const char* getTypeDefine(gpu::ShaderType type) {
		switch (type) {
			case ShaderType::COMPUTE: return "#define LUMIX_COMPUTE_SHADER\n";
			case ShaderType::GEOMETRY: return "#define LUMIX_GEOMETRY_SHADER\n";
			case ShaderType::FRAGMENT: return "#define LUMIX_FRAGMENT_SHADER\n";
			case ShaderType::VERTEX: return "#define LUMIX_VERTEX_SHADER\n";
			default: ASSERT(false); return "";
		}
	}

	static const char* getAttrDefine(u32 idx) {
		switch (idx) {
			case 0 : return "#define _HAS_ATTR0\n";
			case 1 : return "#define _HAS_ATTR1\n";
			case 2 : return "#define _HAS_ATTR2\n";
			case 3 : return "#define _HAS_ATTR3\n";
			case 4 : return "#define _HAS_ATTR4\n";
			case 5 : return "#define _HAS_ATTR5\n";
			case 6 : return "#define _HAS_ATTR6\n";
			case 7 : return "#define _HAS_ATTR7\n";
			case 8 : return "#define _HAS_ATTR8\n";
			case 9 : return "#define _HAS_ATTR9\n";
			case 10 : return "#define _HAS_ATTR10\n";
			case 11 : return "#define _HAS_ATTR11\n";
			case 12 : return "#define _HAS_ATTR12\n";
			default: ASSERT(false); return "";
		}
	}

	TagAllocator m_allocator;
	struct CachedShader {
		CachedShader(IAllocator& allocator) : data(allocator) {}
		OutputMemoryStream data;
		u32 used_srvs_bitset;
		u32 readonly_bitset;
	};
	HashMap<StableHash32, CachedShader> m_cache;
};

} // namespace Lumix::gpu