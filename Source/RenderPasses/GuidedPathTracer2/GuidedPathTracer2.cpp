#include "GuidedPathTracer2.h"
#include "Core/API/Formats.h"
#include "RenderGraph/RenderPassHelpers.h"
#include "RenderGraph/RenderPassLibrary.h"
#include "RenderGraph/RenderPassStandardFlags.h"
#include "Utils/UI/InputTypes.h"
#include <limits>

const RenderPass::Info GuidedPathTracer2::kInfo{"GuidedPathTracer2",
                                                "Guided path tracer."};

// Don't remove this. it's required for hot-reload to function properly
extern "C" FALCOR_API_EXPORT const char *getProjDir() { return PROJECT_DIR; }

extern "C" FALCOR_API_EXPORT void getPasses(Falcor::RenderPassLibrary &lib) {
  lib.registerPass(GuidedPathTracer2::kInfo, GuidedPathTracer2::create);
}

namespace {
const char kCSShaderFile[] =
    "RenderPasses/GuidedPathTracer2/GuidedPathTracer2.cs.slang";
const char kVisVMFShaderFile[] =
    "RenderPasses/GuidedPathTracer2/VisVMF.cs.slang";

const char kInputViewDir[] = "viewW";

const ChannelList kInputChannels = {
    {"vbuffer", "gVBuffer", "Visibility buffer in packed format"},
    {kInputViewDir, "gViewW", "World-space view direction (xyz float format)",
     true /* optional */},
    {"mvec", "gMotion", "Motion vectors"},
};

const ChannelList kOutputChannels = {
    {"color", "gOutputColor", "Output color (sum of direct and indirect)",
     false, ResourceFormat::RGBA32Float},
};

const char kMaxBounces[] = "maxBounces";
const char kComputeDirect[] = "computeDirect";
const char kUseImportanceSampling[] = "useImportanceSampling";
} // namespace

GuidedPathTracer2::SharedPtr
GuidedPathTracer2::create(RenderContext *pRenderContext,
                          const Dictionary &dict) {
  return SharedPtr(new GuidedPathTracer2(dict));
}

GuidedPathTracer2::GuidedPathTracer2(const Dictionary &dict)
    : RenderPass(kInfo) {
  parseDictionary(dict);

  // Create a sample generator.
  mpSampleGenerator = SampleGenerator::create(SAMPLE_GENERATOR_UNIFORM);
  FALCOR_ASSERT(mpSampleGenerator);

  mVisVMF.pState = ComputeState::create();
  mCSTracer.pState = ComputeState::create();

  mpDebugSamples =
      Texture::create2D(2048, 1024, ResourceFormat::RGBA8Unorm, 1, 1, nullptr,
                        Texture::BindFlags::UnorderedAccess);
  mpDebugPdf = Texture::create2D(2048, 1024, ResourceFormat::RGBA8Unorm, 1, 1,
                                 nullptr, Texture::BindFlags::UnorderedAccess);
  mpDebugTile = Texture::create2D(256, 256, ResourceFormat::RGBA8Unorm, 1, 1,
                                  nullptr, Texture::BindFlags::UnorderedAccess);
  mpDebugOutlier =
      Texture::create2D(2048, 1024, ResourceFormat::RGBA8Unorm, 1, 1, nullptr,
                        Texture::BindFlags::UnorderedAccess);
  mpDebugSMISPdf =
      Texture::create2D(2048, 1024, ResourceFormat::RGBA8Unorm, 1, 1, nullptr,
                        Texture::BindFlags::UnorderedAccess);

  mpDebugHead = Buffer::createTyped<uint>(2);
  mpDebugFSamples = Buffer::create(1024 * 1024);
  mpDebugLobes = Buffer::create(1024 * 1024);
}

void GuidedPathTracer2::parseDictionary(const Dictionary &dict) {
  for (const auto &[key, value] : dict) {
    if (key == kMaxBounces)
      mMaxBounces = value;
    else if (key == kComputeDirect)
      mComputeDirect = value;
    else if (key == kUseImportanceSampling)
      mUseImportanceSampling = value;
    else if (key == "transmission")
      mTransmission = value;
    else if (key == "groupExp")
      mSMISGroupExp = value;
    else if (key == "horizontalSMIS")
      mHorizontalSMIS = value;
    else if (key == "shuffle")
      mShuffle = value;
    else if (key == "SPP")
      mSPP = value;
    else if (key == "alpha")
      mParams.MLEMinAlpha = value;
    else
      logWarning("Unknown field '{}' in GuidedPathTracer2 dictionary.", key);
  }
}

Dictionary GuidedPathTracer2::getScriptingDictionary() {
  Dictionary d;
  d[kMaxBounces] = mMaxBounces;
  d[kComputeDirect] = mComputeDirect;
  d[kUseImportanceSampling] = mUseImportanceSampling;
  return d;
}

RenderPassReflection
GuidedPathTracer2::reflect(const CompileData &compileData) {
  RenderPassReflection reflector;

  // Define our input/output channels.
  addRenderPassInputs(reflector, kInputChannels);
  addRenderPassOutputs(reflector, kOutputChannels);

  return reflector;
}

template <typename S, typename T>
auto bindIf(ShaderVar &var, const S &name, const T &t) {
  auto v = var.findMember(name);
  if (!v.isValid())
    return;
  v = t;
}

template <typename S, typename T>
auto bindAssert(ShaderVar &var, const S &name, const T &t) {
  auto v = var.findMember(name);
  FALCOR_ASSERT(v.isValid());
  v = t;
}

void GuidedPathTracer2::execute(RenderContext *pRenderContext,
                                const RenderData &renderData) {
  // Update refresh flag if options that affect the output have changed.
  auto &dict = renderData.getDictionary();
  if (mOptionsChanged) {
    auto flags =
        dict.getValue(kRenderPassRefreshFlags, RenderPassRefreshFlags::None);
    dict[Falcor::kRenderPassRefreshFlags] =
        flags | Falcor::RenderPassRefreshFlags::RenderOptionsChanged;
    mOptionsChanged = false;
  }

  // Get dimensions
  auto dims = renderData.getDefaultTextureDims();
  FALCOR_ASSERT(dims.x > 0 && dims.y > 0);
  mParams.PRNGDimension = dict.keyExists(kRenderPassPRNGDimension)
                              ? dict[kRenderPassPRNGDimension]
                              : 0u;

  Texture::SharedPtr svgfColor;
  mParams.validSVGF = dict.keyExists("SVGFColor");
  if (mParams.validSVGF)
    svgfColor = dict["SVGFColor"];

  if (mParams.dims != dims) {
    mParams.dims = dims;
    uint3 rdims = div_round_up(uint3(dims.x, dims.y, 1), uint3(16, 16, 1)) *
                  uint3(16, 16, 1);

    mpTexMC1[0] =
        Texture::create2D(dims.x, dims.y, ResourceFormat::RGBA32Float, 1, 1,
                          nullptr, ResourceBindFlags::UnorderedAccess);
    mpTexMC1[1] =
        Texture::create2D(dims.x, dims.y, ResourceFormat::RGBA32Float, 1, 1,
                          nullptr, ResourceBindFlags::UnorderedAccess);
    mpTexMC2[0] =
        Texture::create2D(dims.x, dims.y, ResourceFormat::RGBA32Float, 1, 1,
                          nullptr, ResourceBindFlags::UnorderedAccess);
    mpTexMC2[1] =
        Texture::create2D(dims.x, dims.y, ResourceFormat::RGBA32Float, 1, 1,
                          nullptr, ResourceBindFlags::UnorderedAccess);
    mpColPrev =
        Texture::create2D(dims.x, dims.y, ResourceFormat::RGBA32Float, 1, 1,
                          nullptr, ResourceBindFlags::UnorderedAccess);

    mpDebugPos = Buffer::createTyped<float4>(1);

    pRenderContext->clearUAV(mpTexMC1[0]->getUAV().get(), float4(0));
    pRenderContext->clearUAV(mpTexMC1[1]->getUAV().get(), float4(0));
    pRenderContext->clearUAV(mpTexMC2[0]->getUAV().get(), float4(0));
    pRenderContext->clearUAV(mpTexMC2[1]->getUAV().get(), float4(0));
    pRenderContext->clearUAV(mpColPrev->getUAV().get(), float4(0));

    pRenderContext->clearUAV(mpDebugPos->getUAV().get(), float4(0));
  }

  // If we have no scene, just clear the outputs and return.
  if (!mpScene) {
    for (auto it : kOutputChannels) {
      Texture *pDst = renderData.getTexture(it.name).get();
      if (pDst)
        pRenderContext->clearTexture(pDst);
    }
    return;
  }

  if (is_set(mpScene->getUpdates(), Scene::UpdateFlags::GeometryChanged)) {
    throw RuntimeError("GuidedPathTracer2: This render pass does not support "
                       "scene geometry changes.");
  }

  // Request the light collection if emissive lights are enabled.
  if (mpScene->getRenderSettings().useEmissiveLights) {
    mpScene->getLightCollection(pRenderContext);
  }

  // Configure depth-of-field.
  const bool useDOF = mpScene->getCamera()->getApertureRadius() > 0.f;
  if (useDOF && renderData[kInputViewDir] == nullptr) {
    logWarning(
        "Depth-of-field requires the '{}' input. Expect incorrect shading.",
        kInputViewDir);
  }

  auto addDefines = [&](Program &prg) {
    // Specialize program.
    // These defines should not modify the program vars. Do not trigger program
    // vars re-creation.
    prg.addDefine("MAX_BOUNCES", std::to_string(mMaxBounces));
    prg.addDefine("COMPUTE_DIRECT", mComputeDirect ? "1" : "0");
    prg.addDefine("USE_IMPORTANCE_SAMPLING",
                  mUseImportanceSampling ? "1" : "0");
    prg.addDefine("USE_ANALYTIC_LIGHTS",
                  mpScene->useAnalyticLights() ? "1" : "0");
    prg.addDefine("USE_EMISSIVE_LIGHTS",
                  mpScene->useEmissiveLights() ? "1" : "0");
    prg.addDefine("USE_ENV_LIGHT", mpScene->useEnvLight() ? "1" : "0");
    prg.addDefine("USE_ENV_BACKGROUND",
                  mpScene->useEnvBackground() ? "1" : "0");
    prg.addDefine("TRANSMISSION", std::to_string(mTransmission));
    const auto SMISGroupSize = 1 << mSMISGroupExp;
    prg.addDefine("SMIS_GROUP_SIZE", std::to_string(1 << mSMISGroupExp));
    const auto SMISTileYExp = mSMISGroupExp / 2;
    const auto SMISTileY = 1 << SMISTileYExp;
    const auto SMISTileXExp = mSMISGroupExp - SMISTileYExp;
    const auto SMISTileX = 1 << SMISTileXExp;
    prg.addDefine("SMIS_TILE_X", std::to_string(SMISTileY));
    prg.addDefine("SMIS_TILE_Y", std::to_string(SMISTileY));
    auto SPP = mHorizontalSMIS ? mSPP : (1 << mSMISGroupExp);
    prg.addDefine("SPP", std::to_string(SPP));
    prg.addDefine("HORIZONTAL_SMIS", std::to_string(mHorizontalSMIS));
    prg.addDefine("SHUFFLE", std::to_string(mShuffle));

    // For optional I/O resources, set 'is_valid_<name>' defines to
    // inform the program of which ones it can access.
    // TODO: This should be moved to a more general mechanism using
    // Slang.
    prg.addDefines(getValidResourceDefines(kInputChannels, renderData));
    prg.addDefines(getValidResourceDefines(kOutputChannels, renderData));
  };

  addDefines(*mVisVMF.pProgram);
  addDefines(*mCSTracer.pProgram);

  // Prepare program vars. This may trigger shader compilation.
  // The program should have all necessary defines set at this point.
  if (!mCSTracer.pVars)
    prepareVars();
  FALCOR_ASSERT(mCSTracer.pVars);

  {
    int2 gs = mVisVMF.pProgram->getReflector()->getThreadGroupSize();

    mParams.debugMinDim = int2(mParams.debugIdx) - gs / 2;
    mParams.debugMaxDim = mParams.debugMinDim + gs;
  }

  auto bind = [&](ProgramVars &var) {
    auto root = var.getRootVar();

    bindAssert(root, "gScene", mpScene->getParameterBlock());

    auto i = mParams.frameCount % 2;
    bindAssert(root, "gMCStateIn1", mpTexMC1[i]);
    bindAssert(root, "gMCStateIn2", mpTexMC2[i]);
    bindAssert(root, "gMCStateOut1", mpTexMC1[1 - i]);
    bindAssert(root, "gMCStateOut2", mpTexMC2[1 - i]);

    bindAssert(root, "gDebugPos", mpDebugPos);
    bindAssert(root, "gDebugSamples", mpDebugSamples);
    bindAssert(root, "gDebugPdf", mpDebugPdf);
    bindAssert(root, "gDebugTile", mpDebugTile);
    // bindAssert(root, "gDebugOutlier", mpDebugOutlier);
    // bindAssert(root, "gDebugSMISPdf", mpDebugSMISPdf);

    bindAssert(root, "gDebugHead", mpDebugHead);
    bindAssert(root, "gDebugFSamples", mpDebugFSamples);
    bindAssert(root, "gDebugLobes", mpDebugLobes);

    // if (svgfColor)
    //   bindAssert(root, "gSVGFColor", svgfColor);
    bindAssert(root, "gSVGFColor", mpColPrev);

    const auto &pReflector = var.getReflection();
    const auto &pDefaultBlock = pReflector->getDefaultParameterBlock();
    auto pCB =
        var.getParameterBlock(pDefaultBlock->getResourceBinding("ParamsCB"));
    if (pCB)
      pCB->setBlob(&mParams, 0, sizeof(mParams));

    // Bind I/O buffers. These needs to be done per-frame as the buffers may
    // change anytime.
    auto bindChannel = [&](const ChannelDesc &desc) {
      if (!desc.texname.empty())
        bindIf(root, desc.texname, renderData.getTexture(desc.name));
    };
    for (auto channel : kInputChannels)
      bindChannel(channel);
    for (auto channel : kOutputChannels)
      bindChannel(channel);
  };

  if (mDebugReset || mDebugContinous) {
    mDebugReset = false;
    pRenderContext->clearUAV(mpDebugHead->getUAV().get(), float4(0, 0, 0, 0));
  }

  {
    FALCOR_PROFILE("trace");

    bind(*mCSTracer.pVars);
    ShaderVar var = mCSTracer.pVars->getRootVar();
    mpScene->setRaytracingShaderData(pRenderContext, var);
    uint3 numGroups =
        div_round_up(uint3(dims.x, dims.y, 1u),
                     mCSTracer.pProgram->getReflector()->getThreadGroupSize());
    mCSTracer.pState->setProgram(mCSTracer.pProgram);
    pRenderContext->dispatch(mCSTracer.pState.get(), mCSTracer.pVars.get(),
                             numGroups);
  }

  if (mParams.debug == 1) {
    FALCOR_PROFILE("debugVis");

    uint2 dims = uint2(mpDebugPdf->getWidth(), mpDebugPdf->getHeight());

    bind(*mVisVMF.pVars);
    uint3 numGroups =
        div_round_up(uint3(dims.x, dims.y, 1u),
                     mVisVMF.pProgram->getReflector()->getThreadGroupSize());
    mVisVMF.pState->setProgram(mVisVMF.pProgram);
    pRenderContext->dispatch(mVisVMF.pState.get(), mVisVMF.pVars.get(),
                             numGroups);
  }

  pRenderContext->copyResource(mpColPrev.get(),
                               renderData.getTexture("color").get());

  mParams.frameCount++;
}

void GuidedPathTracer2::renderUI(Gui::Widgets &widget) {
  bool dirty = false;

  dirty |= widget.var("Max bounces", mMaxBounces, 0u, 1u << 16);
  widget.tooltip("Maximum path length for indirect illumination.\n0 = direct "
                 "only\n1 = one indirect bounce etc.",
                 true);

  dirty |= widget.checkbox("Evaluate direct illumination", mComputeDirect);
  widget.tooltip("Compute direct illumination.\nIf disabled only indirect is "
                 "computed (when max bounces > 0).",
                 true);

  dirty |= widget.checkbox("Use importance sampling", mUseImportanceSampling);
  widget.tooltip("Use importance sampling for materials", true);

  dirty |= widget.checkbox("Transmission", mTransmission);

  const Gui::DropdownList SMISGroupSizeList = {
      {(uint32_t)0, "1"}, {(uint32_t)1, "2"},  {(uint32_t)2, "4"},
      {(uint32_t)3, "8"}, {(uint32_t)4, "16"}, {(uint32_t)5, "32"},
  };
  dirty |= widget.dropdown("SMIS Group Size", SMISGroupSizeList, mSMISGroupExp);
  dirty |= widget.checkbox("Horizontal SMIS", mHorizontalSMIS);

  if (mHorizontalSMIS)
    dirty |= widget.var("SPP", mSPP);
  else {
    dirty |= widget.checkbox("Shuffle", mShuffle);
  }

  bool discovery = mParams.discovery;
  dirty |= widget.checkbox("Discovery Channel", discovery);
  mParams.discovery = discovery;
  dirty |= widget.var("Discovery Window", mParams.discoveryWindow);
  dirty |= widget.slider("BRDF Sampling Fraction", mParams.brdfSamplingFraction,
                         0.f, 1.f);

  dirty |= widget.var("MLE Max N", mParams.MLEMaxN);
  dirty |= widget.slider("MLE Min Alpha", mParams.MLEMinAlpha, 0.f, 1.f);
  dirty |= widget.var("MLE Prior N", mParams.MLEPriorN, 0.f, 10.f);

  auto dim = [](Texture &tex) -> uint2 {
    return uint2(tex.getWidth(), tex.getHeight());
  };

  bool val = mParams.debug;
  widget.checkbox("Debug", val);
  mParams.debug = val;
  if (mParams.debug) {
    if (widget.button("Reset"))
      mDebugReset = true;
    widget.checkbox("Continous", mDebugContinous);
    widget.image("Samples", mpDebugSamples, dim(*mpDebugSamples));
    widget.slider("Pdf Exposure", mParams.debugPdfExposure, -10.f, 10.f);
    widget.text(std::to_string(std::pow(2.f, mParams.debugPdfExposure)), true);
    widget.image("Pdf", mpDebugPdf, dim(*mpDebugPdf));
    widget.image("Tile", mpDebugTile, dim(*mpDebugTile));
    // widget.var("Outlier Threshold", mParams.debugOutlierThreshold, 0.f);
    // widget.image("Outlier", mpDebugOutlier, dim(*mpDebugOutlier));
    // widget.image("SMISPdf", mpDebugSMISPdf, dim(*mpDebugSMISPdf));
  }

  // If rendering options that modify the output have changed, set flag to
  // indicate that. In execute() we will pass the flag to other passes for reset
  // of temporal data etc.
  if (dirty) {
    mOptionsChanged = true;
  }
}

void GuidedPathTracer2::setScene(RenderContext *pRenderContext,
                                 const Scene::SharedPtr &pScene) {
  // Clear data for previous scene.
  // After changing scene, the raytracing program should to be recreated.
  mCSTracer.pProgram = nullptr;
  mCSTracer.pVars = nullptr;
  mVisVMF.pProgram = nullptr;
  mVisVMF.pVars = nullptr;
  mParams.frameCount = 0;

  // Set new scene.
  mpScene = pScene;

  if (mpScene) {
    if (pScene->hasGeometryType(Scene::GeometryType::Custom)) {
      logWarning("GuidedPathTracer2: This render pass does not support custom "
                 "primitives.");
    }

    {
      Program::Desc desc;
      desc.addShaderModules(mpScene->getShaderModules());
      desc.addShaderLibrary(kCSShaderFile)
          .csEntry("main")
          .setShaderModel("6_5");
      desc.addTypeConformances(mpScene->getTypeConformances());
      desc.setCompilerFlags(Shader::CompilerFlags::TreatWarningsAsErrors |
                            Shader::CompilerFlags::GenerateDebugInfo);

      Program::DefineList defines;
      defines.add(mpScene->getSceneDefines());
      defines.add(mpSampleGenerator->getDefines());

      mCSTracer.pProgram = ComputeProgram::create(desc, defines);
    }

    {
      Program::Desc desc;
      desc.addShaderModules(mpScene->getShaderModules());
      desc.addShaderLibrary(kVisVMFShaderFile)
          .csEntry("main")
          .setShaderModel("6_5");
      desc.addTypeConformances(mpScene->getTypeConformances());
      desc.setCompilerFlags(Shader::CompilerFlags::TreatWarningsAsErrors |
                            Shader::CompilerFlags::GenerateDebugInfo);

      Program::DefineList defines;
      defines.add(mpScene->getSceneDefines());
      defines.add(mpSampleGenerator->getDefines());

      mVisVMF.pProgram = ComputeProgram::create(desc, defines);
    }
  }
}

bool GuidedPathTracer2::onMouseEvent(const MouseEvent &mouseEvent) {
  if (!(mouseEvent.type == MouseEvent::Type::ButtonDown &&
        mouseEvent.button == Input::MouseButton::Right))
    return false;

  mParams.debugRecordPos = 1;
  mParams.debugIdx = uint2(float2(mParams.dims) * mouseEvent.pos);

  return true;
}

void GuidedPathTracer2::prepareVars() {
  FALCOR_ASSERT(mpScene);
  FALCOR_ASSERT(mCSTracer.pProgram);

  // Configure program.
  {
    mCSTracer.pProgram->addDefines(mpSampleGenerator->getDefines());
    mCSTracer.pProgram->setTypeConformances(mpScene->getTypeConformances());

    mCSTracer.pVars = ComputeVars::create(mCSTracer.pProgram->getReflector());

    auto var = mCSTracer.pVars->getRootVar();
    mpSampleGenerator->setShaderData(var);
  }

  mVisVMF.pVars = ComputeVars::create(mVisVMF.pProgram->getReflector());
}
