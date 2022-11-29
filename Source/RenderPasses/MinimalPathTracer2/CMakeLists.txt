add_renderpass(MinimalPathTracer2)

target_sources(MinimalPathTracer2 PRIVATE
    MinimalPathTracer2.cpp
    MinimalPathTracer2.h
    MinimalPathTracer2.rt.slang
)

target_copy_shaders(MinimalPathTracer2 RenderPasses/MinimalPathTracer2)

target_source_group(MinimalPathTracer2 "RenderPasses")
