#pragma once
#include "Utils/HostDeviceShared.slangh"

BEGIN_NAMESPACE_FALCOR

struct Params
{
    uint2 dims = uint2(0, 0);
    uint validSVGF;
    uint frameCount;
    uint PRNGDimension;
    float brdfSamplingFraction = 0.2f;
    uint discovery = 1;
    uint discoveryWindow = 20;
    uint MLEMaxN = 1024;
    float MLEMinAlpha = 0.1f;
    float MLEPriorN = 0.1f;

    float pad01;
    // float pad02;
    // float pad03;

    uint debug = 0;
    uint debugRecordPos = 0;
    uint2 debugIdx;
    int2 debugMinDim;
    int2 debugMaxDim;

    float debugPdfExposure = 1;
    float debugOutlierThreshold = 100;

    // float pad20;
    float pad21;
    float pad22;
};

#ifdef HOST_CODE
static_assert(sizeof(Params) % sizeof(float4) == 0, "CsmData size should be aligned on float4 size");
#endif

END_NAMESPACE_FALCOR
