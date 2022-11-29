/***************************************************************************
 # Copyright (c) 2015-22, NVIDIA CORPORATION. All rights reserved.
 #
 # Redistribution and use in source and binary forms, with or without
 # modification, are permitted provided that the following conditions
 # are met:
 #  * Redistributions of source code must retain the above copyright
 #    notice, this list of conditions and the following disclaimer.
 #  * Redistributions in binary form must reproduce the above copyright
 #    notice, this list of conditions and the following disclaimer in the
 #    documentation and/or other materials provided with the distribution.
 #  * Neither the name of NVIDIA CORPORATION nor the names of its
 #    contributors may be used to endorse or promote products derived
 #    from this software without specific prior written permission.
 #
 # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY
 # EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 # IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 # PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 # CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 # EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 # PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 # PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 # OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 # (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 # OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 **************************************************************************/

/** Minimal path tracer.

    The purpose is to use it for validation of more complex renderers.
    The implementation here should be kept as simple/naive as possible.

    At each hit point (including the primary hit loaded from the V-buffer),
    analytic light sources (point, directional) are sampled uniformly using
    1 shadow ray, and 1 scatter ray is traced to sample the hemisphere.
    At hit/miss the scatter ray includes light from emissive surface and
    the environment map, respectively. Traversal stops at a fixed path length.

    Each type of light (analytic, emissive, env map) can be individually
    enabled/disabled from the host. This clutters the code a bit, but it is
    important as not all other renderes may support all three light types.

    The host sets the following defines:

    MAX_BOUNCES             Maximum number of indirect bounces (0 means no indirect).
    COMPUTE_DIRECT          Nonzero if direct illumination should be included.
    USE_IMPORTANCE_SAMPLING Nonzero if importance sampling should be used for sampling materials.
    USE_ANALYTIC_LIGHTS     Nonzero if Falcor's analytic lights should be used.
    USE_EMISSIVE_LIGHTS     Nonzero if emissive geometry should be used as lights.
    USE_ENV_LIGHT           Nonzero if env map is available and should be used as light source.
    USE_ENV_BACKGROUND      Nonzero if env map is available and should be used as background.
    is_valid_<name>         1 if optional I/O buffer with this name should be used.
*/

#include "Scene/SceneDefines.slangh"
#include "Utils/Math/MathConstants.slangh"

import Scene.Raytracing;
import Scene.Intersection;
import Utils.Math.MathHelpers;
import Utils.Geometry.GeometryHelpers;
import Utils.Sampling.SampleGenerator;
import Rendering.Lights.LightHelpers;
import Params;

cbuffer ParamsCB : register(b0)
{
    Params gParams;
}

// Inputs
Texture2D<PackedHitInfo> gVBuffer;
Texture2D<float4> gViewW; // Optional
Texture2D gMotion;

// Outputs
RWTexture2D<float4> gOutputColor;

// Static configuration based on defines set from the host.
#define is_valid(name) (is_valid_##name != 0)
static const uint kMaxBounces = MAX_BOUNCES;
static const bool kComputeDirect = COMPUTE_DIRECT;
static const bool kUseImportanceSampling = USE_IMPORTANCE_SAMPLING;
static const bool kUseAnalyticLights = USE_ANALYTIC_LIGHTS;
static const bool kUseEmissiveLights = USE_EMISSIVE_LIGHTS;
static const bool kUseEnvLight = USE_ENV_LIGHT;
static const bool kUseEnvBackground = USE_ENV_BACKGROUND;
static const float3 kDefaultBackgroundColor = float3(0, 0, 0);
static const float kRayTMax = FLT_MAX;

/** Payload for shadow ray.
*/
struct ShadowRayData
{
    bool visible;
};

/** Payload for scatter ray (up to 72B).
*/
struct ScatterRayData
{
    PackedHitInfo packed;
};

/** Setup ShadingData based on loaded vertex/material attributes for a hit point.
    \param[in] hit Hit information.
    \param[in] rayOrigin Ray origin.
    \param[in] rayDir Normalized ray direction.
    \param[in] lod Method for computing texture level-of-detail.
    \return ShadingData struct.
*/
ShadingData loadShadingData(const HitInfo hit, const float3 rayOrigin, const float3 rayDir, const ITextureSampler lod)
{
    VertexData v = {};
    uint materialID = {};

#if SCENE_HAS_GEOMETRY_TYPE(GEOMETRY_TYPE_TRIANGLE_MESH)
    if (hit.getType() == HitType::Triangle)
    {
        const TriangleHit triangleHit = hit.getTriangleHit();
        v = gScene.getVertexData(triangleHit);
        materialID = gScene.getMaterialID(triangleHit.instanceID);
    }
#endif
#if SCENE_HAS_GEOMETRY_TYPE(GEOMETRY_TYPE_DISPLACED_TRIANGLE_MESH)
    if (hit.getType() == HitType::DisplacedTriangle)
    {
        const DisplacedTriangleHit displacedTriangleHit = hit.getDisplacedTriangleHit();
        v = gScene.getVertexData(displacedTriangleHit, -rayDir);
        materialID = gScene.getMaterialID(displacedTriangleHit.instanceID);
    }
#endif
#if SCENE_HAS_GEOMETRY_TYPE(GEOMETRY_TYPE_CURVE)
    if (hit.getType() == HitType::Curve)
    {
        const CurveHit curveHit = hit.getCurveHit();
        v = gScene.getVertexDataFromCurve(curveHit);
        materialID = gScene.getMaterialID(curveHit.instanceID);
    }
#endif
#if SCENE_HAS_GEOMETRY_TYPE(GEOMETRY_TYPE_SDF_GRID)
    if (hit.getType() == HitType::SDFGrid)
    {
        const SDFGridHit sdfGridHit = hit.getSDFGridHit();
        v = gScene.getVertexDataFromSDFGrid(sdfGridHit, rayOrigin, rayDir);
        materialID = gScene.getMaterialID(sdfGridHit.instanceID);
    }
#endif

    ShadingData sd = gScene.materials.prepareShadingData(v, materialID, -rayDir, lod);

    return sd;
}

/** Returns the primary ray's direction.
*/
float3 getPrimaryRayDir(uint2 launchIndex, uint2 launchDim, const Camera camera)
{
    if (is_valid(gViewW))
    {
        // If we have the view vector bound as a buffer, just fetch it. No need to compute anything.
        return -gViewW[launchIndex].xyz;
    }
    else
    {
        // Compute the view vector. This must exactly match what the G-buffer pass is doing (jitter etc.).
        // Note that we do not take depth-of-field into account as it would require exactly matching the
        // sample generator between the passes, which is error prone. The host side will issue a warning instead.
        return camera.computeRayPinhole(launchIndex, launchDim).dir;
    }
}

/** Traces a shadow ray towards a light source.
    \param[in] origin Ray origin for the shadow ray.
    \param[in] dir Direction from shading point towards the light source (normalized).
    \param[in] distance Distance to the light source.
    \return True if light is visible, false otherwise.
*/
bool traceShadowRay(float3 origin, float3 dir, float distance)
{
    RayDesc ray;
    ray.Origin = origin;
    ray.Direction = dir;
    ray.TMin = 0.f;
    ray.TMax = distance;

    ShadowRayData rayData;
    rayData.visible = false;    // Set to true by miss shader if ray is not terminated before
    TraceRay(gScene.rtAccel, RAY_FLAG_ACCEPT_FIRST_HIT_AND_END_SEARCH, 0xff /* instanceInclusionMask */, 1 /* hitIdx */, rayTypeCount, 1 /* missIdx */, ray, rayData);

    return rayData.visible;
}

/** Traces a scatter ray based on ray parameters stored in the ray payload.
    \param[in] rayData Describes the ray parameters. The struct is modified based on the result.
*/
HitInfo traceScatterRay(RayDesc ray)
{
    ScatterRayData rayData;
    uint rayFlags = 0;      // TODO: Set cull mode from the app
    TraceRay(gScene.rtAccel, rayFlags, 0xff /* instanceInclusionMask */, 0 /* hitIdx */, rayTypeCount, 0 /* missIdx */, ray, rayData);
    return HitInfo(rayData.packed);
}

/** Evaluates the direct illumination from analytic lights.
    This function samples Falcor's light list uniformly with one shadow ray.
    \param[in] sd Shading data.
    \param[in] bsdf BSDF instance.
    \param[in,out] sg SampleGenerator object.
    \return Outgoing radiance in view direction.
*/
float3 evalDirectAnalytic(const ShadingData sd, const IBSDF bsdf, inout SampleGenerator sg)
{
    const uint lightCount = gScene.getLightCount();
    if (lightCount == 0) return float3(0.f);

    // Pick one of the analytic light sources randomly with equal probability.
    const uint lightIndex = min(uint(sampleNext1D(sg) * lightCount), lightCount - 1);
    float invPdf = lightCount; // Light selection pdf = 1.0 / lightCount.

    // Sample local light source.
    AnalyticLightSample ls;
    if (!sampleLight(sd.posW, gScene.getLight(lightIndex), sg, ls)) return float3(0.f);

    // Reject sample if not in the hemisphere of a BSDF lobe.
    const uint lobes = bsdf.getLobes(sd);
    const bool hasReflection = lobes & uint(LobeType::Reflection);
    const bool hasTransmission = lobes & uint(LobeType::Transmission);
    if (dot(ls.dir, sd.N) <= kMinCosTheta && !hasTransmission) return float3(0.f);
    if (dot(ls.dir, sd.N) >= -kMinCosTheta && !hasReflection) return float3(0.f);

    // Get origin with offset applied in direction of the geometry normal to avoid self-intersection.
    const float3 origin = computeRayOrigin(sd.posW, dot(sd.faceN, ls.dir) >= 0.f ? sd.faceN : -sd.faceN);

    // Test visibility by tracing a shadow ray.
    bool V = traceShadowRay(origin, ls.dir, ls.distance);
    if (!V) return float3(0.f);

    // Evaluate contribution.
    return bsdf.eval(sd, ls.dir, sg) * ls.Li * invPdf;
}

/** Generate a new scatter ray or terminate.
    \param[in] sd Shading data.
    \param[in] bsdf BSDF instance.
    \param[in] isCurveHit True if on curve hit.
    \param[in] rayOrigin Ray origin for the new ray.
    \param[in,out] rayData Ray payload.
    \return True if the path continues.
*/
bool generateScatterRay(uint2 pixel, const ShadingData sd, const IBSDF bsdf, bool isCurveHit, inout SampleGenerator sg, out RayDesc ray, out float3 weight, out float pdf)
{
    ray.Origin = sd.posW;
    ray.TMin = 1e-4;
    ray.TMax = kRayTMax;

    BSDFSample bsdfSample;
    if (bsdf.sample(sd, sg, bsdfSample, kUseImportanceSampling))
    {
        ray.Direction = bsdfSample.wo;
        weight = bsdfSample.weight;
        return any(weight > 0.f);
    }

    return false;
}

/** This is the main entry point for the minimal path tracer.

    One path per pixel is generated, which is traced into the scene.
    The path tracer is written as a for-loop over path segments.

    Built-in light sources (point, directional) are sampled explicitly at each
    path vertex. The contributions from area lights (env map and mesh lights)
    are explicitly added by the scatter ray hit/miss shaders.

    \param[in] pixel Pixel to trace a path for.
    \param[in] frameDim Dimension of the frame in pixels.
    \return Returns the estimated color (radiance).
*/
void tracePath(const uint2 pixel, const uint2 frameDim, out float3 contrib)
{
    contrib = 0.f;
    float3 throughput = float3(1.f);

    // Create sample generator.
    SampleGenerator sg = SampleGenerator(pixel, gParams.frameCount);
    // Advance the generator to the first available dimension.
    // TODO: This is potentially expensive. We may want to store/restore the state from memory if it becomes a problem.
    for (uint i = 0; i < gParams.PRNGDimension; i++) sampleNext1D(sg);

    RayDesc ray;
    ray.Origin = gScene.camera.getPosition();
    ray.Direction = getPrimaryRayDir(pixel, frameDim, gScene.camera);

    HitInfo hit = HitInfo(gVBuffer[pixel]);

    let lod = ExplicitLodTextureSampler(0.f);

    if (gParams.envmap > 0) {
        HitInfo center = HitInfo(gVBuffer[frameDim / 2]);
        ShadingData sd = loadShadingData(center, gScene.camera.getPosition(), getPrimaryRayDir(frameDim / 2, frameDim, gScene.camera), lod);

        ray.Origin = sd.computeNewRayOrigin();
        float2 pnt = (float2(pixel) + sampleNext2D(sg)) / float2(frameDim);
        float theta = pnt.y * M_PI;
        float phi = pnt.x * 2 * M_PI;
        ray.Direction = float3(
            sin(phi) * sin(theta),
            cos(theta),
            cos(phi) * sin(theta)
        );

        if (dot(ray.Direction, sd.faceN) <= 0) return;

        hit = traceScatterRay(ray);
    }

    if (!hit.isValid()) {
        contrib += kUseEnvBackground ? gScene.envMap.eval(ray.Direction) : kDefaultBackgroundColor;
        return;
    }

    // Load shading data.
    ShadingData sd = loadShadingData(hit, ray.Origin, ray.Direction, lod);
    IBSDF bsdf = gScene.materials.getBSDF(sd, lod);

#if TRANSMISSION
    if (bsdf.getProperties(sd).isTransmissive)
    {
        const bool isCurveHit = hit.getType() == HitType::Curve;

        float3 weight;
        float pdf;
        if (!generateScatterRay(pixel, sd, bsdf, isCurveHit, sg, ray, weight, pdf))
            return;

        throughput *= weight;

        HitInfo nextHit = traceScatterRay(ray);

        if (!nextHit.isValid())
        {
            contrib += throughput * (kUseEnvBackground ? gScene.envMap.eval(ray.Direction) : kDefaultBackgroundColor);
            return;
        }

        sd = loadShadingData(nextHit, ray.Origin, ray.Direction, lod);
        bsdf = gScene.materials.getBSDF(sd, lod);
    }
#endif

    if (kComputeDirect)
    {
        contrib += bsdf.getProperties(sd).emission;
    }

    for (uint depth = 0; depth < kMaxBounces + 1 - gParams.envmap; depth++)
    {
        const bool isCurveHit = hit.getType() == HitType::Curve;

        // Add contribution of direct light from analytic lights.
        // Light probe and mesh lights are handled by the scatter ray hit/miss shaders.
        contrib += throughput * (kUseAnalyticLights ? evalDirectAnalytic(sd, bsdf, sg) : float3(0.f));

        // Generate scatter ray.
        float3 weight;
        float pdf;
        if (!generateScatterRay(pixel, sd, bsdf, isCurveHit, sg, ray, weight, pdf))
            return;

        throughput *= weight;

        HitInfo nextHit = traceScatterRay(ray);

        if (!nextHit.isValid()) {
            contrib += throughput * (kUseEnvBackground ? gScene.envMap.eval(ray.Direction) : kDefaultBackgroundColor);
            return;
        }

        // Load shading data.
        ShadingData nextSd = loadShadingData(nextHit, ray.Origin, ray.Direction, lod);
        IBSDF nextBsdf = gScene.materials.getBSDF(nextSd, lod);

#if TRANSMISSION
        if (nextBsdf.getProperties(sd).isTransmissive)
        {
            const bool isCurveHit = hit.getType() == HitType::Curve;

            float3 weight;
            float pdf;
            if (!generateScatterRay(pixel, nextSd, nextBsdf, isCurveHit, sg, ray, weight, pdf))
                return;

            throughput *= weight;

            HitInfo nextHit = traceScatterRay(ray);

            if (!nextHit.isValid())
            {
                contrib += throughput * (kUseEnvBackground ? gScene.envMap.eval(ray.Direction) : kDefaultBackgroundColor);
                return;
            }

            nextSd = loadShadingData(nextHit, ray.Origin, ray.Direction, lod);
            nextBsdf = gScene.materials.getBSDF(nextSd, lod);
        }
#endif

        let emission = nextBsdf.getProperties(nextSd).emission;
        contrib += throughput * emission;

        hit = nextHit;
        sd = nextSd;
        bsdf = nextBsdf;
    }
}

//
// Shader entry points for miss shaders.
//

[shader("miss")]
void scatterMiss(inout ScatterRayData rayData)
{
    rayData.packed = HitInfo().getData();
}

[shader("miss")]
void shadowMiss(inout ShadowRayData rayData)
{
    // The miss shader is executed if the ray misses all geometry. Mark as visible.
    rayData.visible = true;
}

//
// Shader entry points for TriangleMesh hit groups.
//

[shader("anyhit")]
void scatterTriangleMeshAnyHit(inout ScatterRayData rayData, BuiltInTriangleIntersectionAttributes attribs)
{
    // Alpha test for non-opaque geometry.
    GeometryInstanceID instanceID = getGeometryInstanceID();
    VertexData v = getVertexData(instanceID, PrimitiveIndex(), attribs);
    const uint materialID = gScene.getMaterialID(instanceID);
    //if (gScene.materials.alphaTest(v, materialID, 0.f)) IgnoreHit();
}

[shader("closesthit")]
void scatterTriangleMeshClosestHit(inout ScatterRayData rayData, BuiltInTriangleIntersectionAttributes attribs)
{
    TriangleHit triangleHit;
    triangleHit.instanceID = getGeometryInstanceID();
    triangleHit.primitiveIndex = PrimitiveIndex();
    triangleHit.barycentrics = attribs.barycentrics;
    rayData.packed = HitInfo(triangleHit).getData();
}

[shader("anyhit")]
void shadowTriangleMeshAnyHit(inout ShadowRayData rayData, BuiltInTriangleIntersectionAttributes attribs)
{
    // Alpha test for non-opaque geometry.
    GeometryInstanceID instanceID = getGeometryInstanceID();
    VertexData v = getVertexData(instanceID, PrimitiveIndex(), attribs);
    const uint materialID = gScene.getMaterialID(instanceID);
    if (gScene.materials.alphaTest(v, materialID, 0.f)) IgnoreHit();
}

//
// Shader entry points for DisplacedTriangleMesh hit groups.
//

[shader("intersection")]
void displacedTriangleMeshIntersection()
{
    const Ray ray = Ray(WorldRayOrigin(), WorldRayDirection(), RayTMin(), RayTCurrent());
    DisplacedTriangleMeshIntersector::Attribs attribs;
    float t;
    if (DisplacedTriangleMeshIntersector::intersect(ray, getGeometryInstanceID(), PrimitiveIndex(), attribs, t))
    {
        ReportHit(t, 0, attribs);
    }
}

[shader("closesthit")]
void scatterDisplacedTriangleMeshClosestHit(inout ScatterRayData rayData, DisplacedTriangleMeshIntersector::Attribs attribs)
{
    DisplacedTriangleHit displacedTriangleHit;
    displacedTriangleHit.instanceID = getGeometryInstanceID();
    displacedTriangleHit.primitiveIndex = PrimitiveIndex();
    displacedTriangleHit.barycentrics = attribs.barycentrics;
    displacedTriangleHit.displacement = attribs.displacement;
    rayData.packed = HitInfo(displacedTriangleHit).getData();
}

//
// Shader entry points for Curve hit groups.
//

[shader("intersection")]
void curveIntersection()
{
    const Ray ray = Ray(WorldRayOrigin(), WorldRayDirection(), RayTMin(), RayTCurrent());
    CurveIntersector::Attribs attribs;
    float t;
    if (CurveIntersector::intersect(ray, getGeometryInstanceID(), PrimitiveIndex(), attribs, t))
    {
        ReportHit(t, 0, attribs);
    }
}

[shader("closesthit")]
void scatterCurveClosestHit(inout ScatterRayData rayData, CurveIntersector::Attribs attribs)
{
    CurveHit curveHit;
    curveHit.instanceID = getGeometryInstanceID();
    curveHit.primitiveIndex = PrimitiveIndex();
    curveHit.barycentrics = attribs.barycentrics;
    rayData.packed = HitInfo(curveHit).getData();
}

//
// Shader entry points for SDFGrid hit groups.
//

[shader("intersection")]
void sdfGridIntersection()
{
    const Ray ray = Ray(WorldRayOrigin(), WorldRayDirection(), RayTMin(), RayTCurrent());
    SDFGridHitData sdfGridHitData;
    float t;
    if (SDFGridIntersector::intersect(ray, getGeometryInstanceID(), PrimitiveIndex(), sdfGridHitData, t))
    {
        ReportHit(t, 0, sdfGridHitData);
    }
}

[shader("closesthit")]
void scatterSdfGridClosestHit(inout ScatterRayData rayData, SDFGridHitData sdfGridHitData)
{
    SDFGridHit sdfGridHit;
    sdfGridHit.instanceID = getGeometryInstanceID();
    sdfGridHit.hitData = sdfGridHitData;
    rayData.packed = HitInfo(sdfGridHit).getData();
}

//
// Shader entry point for ray generation shader.
//

[shader("raygeneration")]
void rayGen()
{
    uint2 pixel = DispatchRaysIndex().xy;
    uint2 frameDim = DispatchRaysDimensions().xy;

    float3 color;
    tracePath(pixel, frameDim, color);

    gOutputColor[pixel] = float4(color, 1.f);
}
