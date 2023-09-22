#pragma once

#include "renderer/renderer.h"

namespace Lumix::gpu {

RenderPlugin* createFSR2RenderPlugin(IAllocator& allocator);
void destroyFSR2RenderPlugin();

} // namespace Lumix::gpu