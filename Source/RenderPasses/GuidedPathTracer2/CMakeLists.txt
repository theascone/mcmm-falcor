add_renderpass(GuidedPathTracer2)

target_sources(GuidedPathTracer2 PRIVATE
    GuidedPathTracer2.cpp
    GuidedPathTracer2.h
    GuidedPathTracer2.cs.slang
    VisVMF.cs.slang
    Bindings.slangh
    Common.slangh
    Params.slang
    Reservoir.slangh
    VMF.slangh
)

target_copy_shaders(GuidedPathTracer2 RenderPasses/GuidedPathTracer2)

target_source_group(GuidedPathTracer2 "RenderPasses")
