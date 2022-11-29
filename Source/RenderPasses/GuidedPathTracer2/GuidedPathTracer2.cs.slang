#include "Bindings.slangh"
#include "Common.slangh"
#include "Utils/Math/MathConstants.slangh"

#include "Reservoir.slangh"
#include "VMF.slangh"

import Scene.RaytracingInline;
import Utils.Color.ColorHelpers;
import Utils.Math.MathHelpers;
import Utils.Sampling.SampleGenerator;
import Rendering.Lights.LightHelpers;
import Rendering.Materials.StandardBSDF;
import Params;

#define MASK (SMIS_GROUP_SIZE - 1)
#define SCOPE_BEGIN \
    do
#define SCOPE_END \
    while (false) \
        ;

static const uint kMaxBounces = MAX_BOUNCES;
static const bool kComputeDirect = COMPUTE_DIRECT;
static const bool kUseImportanceSampling = USE_IMPORTANCE_SAMPLING;
static const bool kUseAnalyticLights = USE_ANALYTIC_LIGHTS;
static const bool kUseEmissiveLights = USE_EMISSIVE_LIGHTS;
static const bool kUseEnvLight = USE_ENV_LIGHT;
static const bool kUseEnvBackground = USE_ENV_BACKGROUND;
static const float3 kDefaultBackgroundColor = float3(0, 0, 0);
static const float kRayTMax = FLT_MAX;

/** Traces a scatter ray based on ray parameters stored in the ray payload.
    \param[in] rayData Describes the ray parameters. The struct is modified based on the result.
*/
HitInfo traceScatterRay(Ray ray, bool valid = true)
{
    HitInfo hit;
    float hitT;
    uint rayFlags = 0; // TODO: Set cull mode from the app

    SceneRayQuery<0> sceneRayQuery;
    sceneRayQuery.traceRay(ray, hit, hitT, rayFlags, valid ? 0xff : 0);

    return hit;
}

float mc_state_C(in MCStateMLE s, in ShadingData sd)
{
    return dot(s.getDirection(sd.posW), sd.N) > 0;
}

float mc_state_score(in MCStateMLE s, ShadingData sd)
{
    return s.f; // * mc_state_C(s, sd);
}

void read_neighbor_state(
    inout MCStateMLE tent,
    inout SampleGenerator sg,
    in ShadingData sd,
    in int2 repr)
{
    float sum = mc_state_score(tent, sd);
    for (int i = 0; i < 5; i++)
    {
        int2 other = repr - 0.5 * gParams.discoveryWindow + sampleNext2D(sg) * gParams.discoveryWindow;
        if (all(0 <= other && other < gParams.dims))
        {
            MCStateMLE tent2 = MCStateMLE(gMCStateIn1[other], gMCStateIn2[other]);
            float score = mc_state_score(tent2, sd);
            sum += score;
            if (sum > 0 && sampleNext1D(sg) < score / sum)
                tent = tent2;
        }
    }
}

/** This is the main entry point for the minimal path tracer.

    One path per pixel is generated, which is traced into the scene.
    The path tracer is written as a for-loop over path segments.

    Built-in light sources (point, directional) are sampled explicitly at each
    path vertex. The contributions from area lights (env map and mesh lights)
    are explicitly added by the scatter ray hit/miss shaders.

    \param[in] pixel Pixel to trace a path for.
    \return Returns the estimated color (radiance).
*/
void tracePath(const uint2 pixel, out float3 contrib)
{
    bool valid = all(pixel < gParams.dims);

    contrib = 0.f;
    float3 throughput = float3(1.f);

    // Create sample generator.
    SampleGenerator sg = SampleGenerator(pixel, gParams.frameCount);
    // Advance the generator to the first available dimension.
    // TODO: This is potentially expensive. We may want to store/restore the state from memory if it becomes a problem.
    for (uint i = 0; i < gParams.PRNGDimension; i++)
        sampleNext1D(sg);

    let lod = ExplicitLodTextureSampler(0.f);

    float3 primaryDir = getPrimaryRayDir(pixel, gParams.dims, gScene.camera);

    int2 repr = pixel;
    if (valid)
    {
        const int2 offset[8] = {
            int2(-1, -1),
            int2(-1, 1),
            int2(1, -1),
            int2(1, 1),
            int2(1, 0),
            int2(0, -1),
            int2(0, 1),
            int2(-1, 0),
        };

        float2 motion = gMotion.Load(int3(pixel, 0)).xy;
        [unroll]
        for (int a = 0; a < 8; a++)
        {
            float2 m = gMotion.Load(int3(pixel + offset[a], 0)).rg;
            motion = dot(m, m) > dot(motion, motion) ? m : motion;
        }

        repr = pixel + motion * gParams.dims + float2(0.5, 0.5);
    }

    HitInfo hit = {};
    if (valid)
        hit = HitInfo(gVBuffer[pixel]);
    if (valid && !hit.isValid())
    {
        contrib += kUseEnvBackground ? gScene.envMap.eval(primaryDir) : kDefaultBackgroundColor;
        valid = false;
    }

    // Load shading data.
    ShadingData sd;
    IBSDF bsdf;

    if (valid)
    {
        sd = loadShadingData(hit, gScene.camera.getPosition(), primaryDir, lod);
        bsdf = gScene.materials.getBSDF(sd, lod);
#if TRANSMISSION
        if (bsdf.getProperties(sd).isTransmissive)
        {
            valid = false;
            SCOPE_BEGIN
            {
                BSDFSample bsdfSample;
                if (!bsdf.sample(sd, sg, bsdfSample, kUseImportanceSampling))
                    break;

                Ray ray;
                ray.origin = sd.posW;
                ray.tMin = 1e-4;
                ray.tMax = kRayTMax;
                ray.dir = bsdfSample.wo;
                throughput *= bsdfSample.weight;
                hit = traceScatterRay(ray);

                if (!hit.isValid())
                {
                    contrib += throughput * (kUseEnvBackground ? gScene.envMap.eval(ray.dir) : kDefaultBackgroundColor);
                    break;
                }
                sd = loadShadingData(hit, ray.origin, ray.dir, lod);
                bsdf = gScene.materials.getBSDF(sd, lod);
                valid = true;
            }
            SCOPE_END
        }
#endif
    }

    if (valid && gParams.debugRecordPos > 0 && all(gParams.debugIdx == pixel))
        gDebugPos[0] = float4(sd.posW, 1);

    if (valid && kComputeDirect)
        contrib += throughput * bsdf.getProperties(sd).emission;

    MCStateMLE curr = MCStateMLE();
    MCStateMLE tent = MCStateMLE();
    MCStateMLE ptent = MCStateMLE();

#if !HORIZONTAL_SMIS
    VMF vmfs[SPP];
    float3 sample_dirs[SPP];
    float3 sample_weights[SPP];
#endif

    for (uint i = 0; i < SPP; i++)
    {
#if !HORIZONTAL_SMIS
#if SHUFFLE
        tent.waveShuffle((WaveGetLaneIndex() & ~MASK) | ((WaveGetLaneIndex() + 1) & MASK));
#endif
        sample_dirs[i] = float3(0);
        sample_weights[i] = float3(0);
#endif
        // use_vmf = sampleNext1D(sg) >= gParams.brdfSamplingFraction;
        // if (use_vmf)
        if (valid && gParams.discovery)
            read_neighbor_state(tent, sg, sd, repr);

#if HORIZONTAL_SMIS
        ptent = tent;
        uint mask = WaveReadLaneFirst(min(sampleNext1D(sg) * SMIS_GROUP_SIZE, SMIS_GROUP_SIZE - 1));
        tent.waveShuffle(WaveGetLaneIndex() ^ mask);
#endif

        VMF vmf = VMF();
        if (valid && tent.valid())
            vmf = tent.vmf(sd.posW);

#if !HORIZONTAL_SMIS
        vmfs[i] = vmf;
#endif

        float pdf = 0;
        float3 directContrib = float3(0);
        float3 direction = float3(0);
        float3 position = float3(0);
        bool bsdf_sample = false;

        bool spec = false;

        SCOPE_BEGIN
        {
            float3 throughput = float3(0);

            if (!vmf.valid() || sampleNext1D(sg) < gParams.brdfSamplingFraction)
            {
                BSDFSample bsdfSample;
                if (!bsdf.sample(sd, sg, bsdfSample, kUseImportanceSampling))
                    break;

                if ((bsdfSample.isLobe(LobeType::SpecularReflection) && bsdf.getProperties(sd).roughness < 0.08)
                     || bsdfSample.isLobe(LobeType::DeltaReflection))
                    spec = true;

                direction = bsdfSample.wo;
                pdf = bsdfSample.pdf;
                throughput = bsdfSample.weight;

                bsdf_sample = true;
            }
            else
            {
                direction = vmf.sample(sampleNext2D(sg));
                pdf = vmf.pdf(direction);
                throughput = bsdf.eval(sd, direction, sg) / pdf;
            }

            Ray ray;
            ray.origin = sd.posW;
            ray.tMin = 1e-4;
            ray.dir = direction;
            ray.tMax = kRayTMax;
            HitInfo nextHit = traceScatterRay(ray, pdf > 0 || spec);

            if (!nextHit.isValid())
                break;

            ShadingData nextSd = loadShadingData(nextHit, ray.origin, ray.dir, lod);
            IBSDF nextBsdf = gScene.materials.getBSDF(nextSd, lod);

            position = nextSd.posW;
#if TRANSMISSION
            let prop = nextBsdf.getProperties(nextSd);
            if (nextBsdf.getProperties(nextSd).isTransmissive)
            {

                BSDFSample bsdfSample;
                if (!nextBsdf.sample(nextSd, sg, bsdfSample, kUseImportanceSampling))
                    break;
                float comp = abs(dot(nextSd.N, ray.dir) / dot(nextSd.N, bsdfSample.wo)) * prop.eta;
                throughput *= bsdfSample.weight;
                ray.origin = nextSd.posW;
                ray.dir = bsdfSample.wo;
                nextHit = traceScatterRay(ray);
                if (!nextHit.isValid())
                    break;
                nextSd = loadShadingData(nextHit, ray.origin, ray.dir, lod);
                nextBsdf = gScene.materials.getBSDF(nextSd, lod);

                position += normalize(direction) * distance(nextSd.posW, position) * comp;
            }
#endif

            directContrib = throughput * nextBsdf.getProperties(nextSd).emission;
        }
        SCOPE_END

#if HORIZONTAL_SMIS
        {
            let bsdf_pdf = bsdf.evalPdf(sd, direction, kUseImportanceSampling);

            float sum_pdf = 0;

            [unroll]
            for (int t = 0; t < SMIS_GROUP_SIZE; t++)
            {
                VMF ovmf = ptent.waveReadLaneAt(WaveGetLaneIndex() & ~MASK | t).vmf(sd.posW);
                sum_pdf += ovmf.valid() ? ovmf.pdf(direction) : bsdf_pdf;
            }

            let f = gParams.brdfSamplingFraction;
            sum_pdf = f * SMIS_GROUP_SIZE * bsdf_pdf + (1 - f) * sum_pdf;

            if (sum_pdf > 0)
            {
                float3 con = SMIS_GROUP_SIZE * throughput * directContrib * pdf / (sum_pdf * SPP);
                if (!any(isnan(con) || any(isinf(con))))
                    contrib += con;
            }
        }
#else
        sample_dirs[i] = direction;
        sample_weights[i] = throughput * directContrib * pdf;
        if (spec) {
            float3 con = throughput * directContrib / gParams.brdfSamplingFraction;

            if (!any(isnan(con) || isinf(con)))
                contrib += con;
            sample_weights[i] = 0;
            continue;
        }
#endif

        if (!valid)
            continue;

        float tent_f = luminance(throughput * directContrib);

        float oldf = 0;
        if (gParams.validSVGF && all(0 <= repr) && all(repr < gParams.dims))
            oldf = luminance(gSVGFColor[repr].xyz);

        float cmp = curr.f;

        if (cmp == 0 || sampleNext1D(sg) < tent_f / cmp)
        {
            if (bsdf_sample)
                tent = MCStateMLE();
            tent.f = tent_f;
            tent.add(sd.posW, tent_f, direction, position);
            curr = tent;
        }
        else if (!bsdf_sample)
            tent.add(sd.posW, tent_f, direction, position);
    }

#if !HORIZONTAL_SMIS
    for (int s = 0; s < SPP; s++)
    {
        let dir = sample_dirs[s];
        let weight = sample_weights[s];

        if (all(weight == 0))
            continue;

        let bsdf_pdf = bsdf.evalPdf(sd, dir, kUseImportanceSampling);

        float sum_pdf = 0;

        for (int t = 0; t < SPP; t++)
        {
            let vmf = vmfs[t];
            sum_pdf += vmf.valid() ? vmf.pdf(dir) : bsdf_pdf;
        }

        let f = gParams.brdfSamplingFraction;
        sum_pdf = f * SMIS_GROUP_SIZE * bsdf_pdf + (1 - f) * sum_pdf;

        if (sum_pdf > 0)
        {
            float3 con = weight / sum_pdf;
            if (!any(isnan(con) || any(isinf(con))))
                contrib += con;
        }
    }
#endif
    if (valid)
        curr.pack(gMCStateOut1[pixel], gMCStateOut2[pixel]);
}

//
// Shader entry point for ray generation shader.
//

#define GROUP_X 16
#define GROUP_Y 16

[numthreads(GROUP_X, GROUP_Y, 1)]
void main(uint groupThreadIndex: SV_GroupIndex, uint3 groupId: SV_GroupID)
{
    let SMIS_TILES_X = GROUP_X / SMIS_TILE_X;
    let SMIS_TILES_Y = GROUP_Y / SMIS_TILE_Y;

    let tileIndex = groupThreadIndex / (SMIS_TILE_X * SMIS_TILE_Y);
    let tileId = uint2(tileIndex % SMIS_TILES_X, tileIndex / SMIS_TILES_Y);
    let tileThreadIndex = groupThreadIndex % (SMIS_TILE_X * SMIS_TILE_Y);
    let tileThreadId = uint2(tileThreadIndex % SMIS_TILE_X, tileThreadIndex / SMIS_TILE_X);

    let pixel = groupId.xy * uint2(GROUP_X, GROUP_Y) +
                tileId * uint2(SMIS_TILE_X, SMIS_TILE_Y) +
                tileThreadId;

    float3 color;
    tracePath(pixel, color);

    if (false)
    {
        float lum = luminance(color);
        color = float3(lum, lum, lum);
    }

    if (all(pixel <= gParams.dims))
        gOutputColor[pixel] = float4(color, 1.f);
}
