#pragma once

#include "renderer/gpu/gpu.h"

namespace Lumix::gpu {

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

struct FSR2Context;

FSR2Context* fsr2Init(u32 w, u32 h, IAllocator& allocator);
bool fsr2Dispatch(FSR2Context& ctx, const FSR2DispatchParams& params);
void fsr2Shutdown(FSR2Context& ctx);


} // namespace Lumix::gpu