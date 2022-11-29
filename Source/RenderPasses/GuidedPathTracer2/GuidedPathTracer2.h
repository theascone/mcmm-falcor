#pragma once
#include "Core/Program/ComputeProgram.h"
#include "Falcor.h"
#include "Utils/Sampling/SampleGenerator.h"

#include "Params.slang"

using namespace Falcor;

/** Guided path tracer.

    This pass implements a guided brute-force path tracer. It does purposely
    not use any importance sampling or other variance reduction techniques.
    The output is unbiased/consistent ground truth images, against which other
    renderers can be validated.

    Note that transmission and nested dielectrics are not yet supported.
*/
class GuidedPathTracer2 : public RenderPass {
public:
  using SharedPtr = std::shared_ptr<GuidedPathTracer2>;

  static const Info kInfo;

  static SharedPtr create(RenderContext *pRenderContext = nullptr,
                          const Dictionary &dict = {});

  virtual Dictionary getScriptingDictionary() override;
  virtual RenderPassReflection reflect(const CompileData &compileData) override;
  virtual void execute(RenderContext *pRenderContext,
                       const RenderData &renderData) override;
  virtual void renderUI(Gui::Widgets &widget) override;
  virtual void setScene(RenderContext *pRenderContext,
                        const Scene::SharedPtr &pScene) override;
  virtual bool onMouseEvent(const MouseEvent &mouseEvent) override;
  virtual bool onKeyEvent(const KeyboardEvent &keyEvent) override {
    return false;
  }

private:
  GuidedPathTracer2(const Dictionary &dict);
  void parseDictionary(const Dictionary &dict);
  void prepareVars();

  // Internal state
  Scene::SharedPtr mpScene;                     ///< Current scene.
  SampleGenerator::SharedPtr mpSampleGenerator; ///< GPU sample generator.

  Params mParams;

  // Configuration
  uint mMaxBounces = 3; ///< Max number of indirect bounces (0 = none).
  bool mComputeDirect =
      true; ///< Compute direct illumination (otherwise indirect only).
  bool mUseImportanceSampling =
      true; ///< Use importance sampling for materials.

  bool mTransmission = 0;
  uint mSMISGroupExp = 3;
  bool mHorizontalSMIS = 0;
  bool mShuffle = 1;
  uint mSPP = 8;

  // Runtime data
  bool mOptionsChanged = false;
  bool mDebugContinous = false;
  bool mDebugReset = false;

  // Buffers
  Texture::SharedPtr mpTexMC1[2];
  Texture::SharedPtr mpTexMC2[2];
  Texture::SharedPtr mpColPrev;

  Buffer::SharedPtr mpDebugPos;
  Texture::SharedPtr mpDebugSamples;
  Texture::SharedPtr mpDebugPdf;
  Texture::SharedPtr mpDebugTile;
  Texture::SharedPtr mpDebugOutlier;
  Texture::SharedPtr mpDebugSMISPdf;

  Buffer::SharedPtr mpDebugHead;
  Buffer::SharedPtr mpDebugFSamples;
  Buffer::SharedPtr mpDebugLobes;

  struct {
    ComputeProgram::SharedPtr pProgram;
    ComputeVars::SharedPtr pVars;
    ComputeState::SharedPtr pState;
  } mCSTracer;

  struct {
    ComputeProgram::SharedPtr pProgram;
    ComputeVars::SharedPtr pVars;
    ComputeState::SharedPtr pState;
  } mVisVMF;
};
