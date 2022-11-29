#include "Bindings.slangh"
#include "Common.slangh"
#include "Reservoir.slangh"
#include "Utils/Math/MathConstants.slangh"
#include "VMF.slangh"

import Scene.RaytracingInline;
import Falcor.Utils.Color.ColorHelpers;

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

bool validateNum(float x)
{
    return isnan(x) || isinf(x);
}

[numthreads(16, 16, 1)]
void main(uint3 dispatchThreadId: SV_DispatchThreadID)
{
    let pixel = gParams.debugIdx;
    const uint2 id = dispatchThreadId.xy;

    uint2 dims;
    gDebugPdf.GetDimensions(dims.x, dims.y);

    VertexData vd;
    {

        HitInfo hit = HitInfo(gVBuffer[pixel]);
        if (!hit.isValid())
            return;

        RayDesc ray;
        ray.Origin = gScene.camera.getPosition();
        ray.Direction = getPrimaryRayDir(pixel, gParams.dims, gScene.camera);

        let lod = ExplicitLodTextureSampler(0.f);

        vd = loadVertexData(hit, ray.Origin, ray.Direction, lod);
    }

    gDebugPdf[id] = float4(0, 0, 0, 1);

    if (all(id < dims))
    {
        float emission = 0;
        float3 direction = pnt2dir(float2(id) / dims);

        let F = 4;

        for (int y = 0; y < F; y++)
            for (int x = 0; x < F; x++)
            {
                float3 direction = pnt2dir((F * float2(id) + float2(x, y)) / (F * dims));
                Ray ray;
                ray.origin = vd.posW;
                ray.dir = direction;
                ray.tMin = 1e-5;
                ray.tMax = FLT_MAX;
                HitInfo hit = traceScatterRay(ray);

                if (hit.isValid())
                {
                    let lod = ExplicitLodTextureSampler(0.f);
                    let sd = loadShadingData(hit, gScene.camera.getPosition(), direction, lod);
                    let bsdf = gScene.materials.getBSDF(sd, lod);
                    emission += any(bsdf.getProperties(sd).emission) > 0 ? 1. / (F * F) : 0;
                }
            }

        {
            float N = 0;
            float acc = 0.f;
            for (int y = gParams.debugMinDim.y; y < gParams.debugMaxDim.y; y++)
            {
                for (int x = gParams.debugMinDim.x; x < gParams.debugMaxDim.x; x++)
                {
                    uint2 idx = uint2(x, y);
                    if (!(all(0 <= idx) && all(idx < gParams.dims)))
                        continue;

                    VMF lobe = MCStateMLE(gMCStateOut1[idx], gMCStateOut2[idx]).vmf(vd.posW);

                    float2 diff_ = dir2pnt(lobe.direction) * dims - id;
                    if (length(diff_) < 1)
                    {
                        acc += 1.f;
                        N++;
                    }
                }
            }

            gDebugSamples[id] = float4(acc / N, acc / N, acc / N, 1);
        }

        if (true)
        {
            float pdf = 0;
            uint n = 0;
            for (int y = gParams.debugMinDim.y; y < gParams.debugMaxDim.y; y++)
            {
                for (int x = gParams.debugMinDim.x; x < gParams.debugMaxDim.x; x++)
                {
                    // if (x != 0) continue;
                    uint2 idx = uint2(x, y);
                    if (!(all(0 <= idx) && all(idx < gParams.dims)))
                        continue;

                    let state = MCStateMLE(gMCStateOut1[idx], gMCStateOut2[idx]);

                    if (state.f == 0)
                        continue;

                    let lobe = state.vmf(vd.posW);

                    if (!lobe.valid())
                        continue;

                    n++;
                    if (x == pixel.x && y == pixel.y)
                    {
                        pdf += lobe.pdf(direction);
                        pdf = lobe.cosQuantile(0.5) < dot(direction, lobe.direction);
                        // float2 diff_ = dir2pnt(lobe.direction) * dims - id;
                        // if (length(diff_) < 1)
                        //{
                        //     acc2 += 1;
                        // }
                    }

                    // float2 diff_ = dir2pnt(lobe.direction) * dims - id;
                    // if (length(diff_) < 1)
                    //{
                    //     acc1 += 1;
                    // }

                    // acc += lobe.pdf(direction);
                    // acc1 += 1;
                }
            }

            float maxVal = pow(2, gParams.debugPdfExposure);
            // float3 col = emission > 0 ? float3(emission, 0, 0) : float3(1 - clamp(pdf / n / maxVal, 0, 1));
            // gDebugPdf[id] = float4(col, 1);

            // float t = clamp(pdf / n / maxVal, 0, 1);
            float t = clamp(pdf, 0, 1);
            float3 color;
            if (emission > 0)
                color = lerp(float3(0, 0, 0), lerp(float3(0, 1, 0), float3(1, 1, 1), 0.0), t);
            else
                color = lerp(float3(1, 1, 1), lerp(float3(1, 0, 0), float3(1, 1, 1), 0.0), t);
            gDebugPdf[id] = float4(color, 1);

            //                gDebugPdf[id] = float4(color_map() * (1 - 0.75 * emission), 1);

            // if (acc2 > 0)
            //{
            //     acc = 0;
            //     acc1 = 0;
            // }
            // float3 color = float3(acc2, acc, acc1);
            // color += linearToSRGB(emission * 0.01);
            // color = float3(emission, min(1, acc * 1), 0);
            // gDebugPdf[id] = float4(color, 1);
            // acc /= acc1;

            // gDebugPdf[id] = float4(color_map(clamp(acc / maxVal, 0, 1)), 1);
        }

        if (false)
        {
            float acc = 0.f;
            float acc1 = 0.f;
            float acc2 = 0.f;
            for (int y = gParams.debugMinDim.y; y < gParams.debugMaxDim.y; y++)
            {
                for (int x = gParams.debugMinDim.x; x < gParams.debugMaxDim.x; x++)
                {
                    // if (x != 0) continue;
                    uint2 idx = uint2(x, y);
                    if (!(all(0 <= idx) && all(idx < gParams.dims)))
                        continue;

                    let state = MCStateMLE(gMCStateOut1[idx], gMCStateOut2[idx]);

                    if (state.f == 0)
                        continue;

                    let lobe = state.vmf(vd.posW);

                    if (!lobe.valid())
                        continue;

                    if (x == pixel.x && y == pixel.y)
                    {
                        acc += lobe.cosQuantile(0.5) < dot(direction, lobe.direction);

                        // float2 diff_ = dir2pnt(lobe.direction) * dims - id;
                        // if (length(diff_) < 1)
                        //{
                        //     acc2 += 1;
                        // }
                    }

                    // float2 diff_ = dir2pnt(lobe.direction) * dims - id;
                    // if (length(diff_) < 1)
                    //{
                    //     acc1 += 1;
                    // }

                    // acc += lobe.pdf(direction);
                    // acc1 += 1;
                }
            }

            // if (acc2 > 0)
            //{
            //     acc = 0;
            //     acc1 = 0;
            // }
            float3 color = float3(acc2, acc, acc1);
            color += linearToSRGB(emission * 0.01);
            color = float3(emission, min(1, acc * 1), 0);
            gDebugPdf[id] = float4(color, 1);
            // acc /= acc1;

            float maxVal = pow(2, gParams.debugPdfExposure);
            // gDebugPdf[id] = float4(color_map(clamp(acc / maxVal, 0, 1)), 1);
        }
    }

    gDebugTile.GetDimensions(dims.x, dims.y);

    uint2 diff = gParams.debugMaxDim - gParams.debugMinDim;

    if (all(id < dims))
    {
        float4 color = float4(0, 0, 0, 1);
        int2 idx = gParams.debugMinDim + id / 16;
        if (all(0 <= idx) && all(idx < gParams.dims))
        {
            let state = MCStateMLE(gMCStateOut1[idx], gMCStateOut2[idx]);

            color.r = float(validateNum(state.sumOfWeightedDirectionsLength));
        }

        gDebugTile[id] = color;
    }

    /*
    {
        float3 direction = pnt2dir(float2(id) / dims);
        float acc = 0.f;

        uint size, stride;
        gDebugFSamples.GetDimensions(size, stride);
        size = min(size, gDebugHead[0]);

        size = min(size, 1000);

        for (uint x = 0; x < size; x++)
        {
            float3 dir = gDebugFSamples[x];
            float2 diff_ = dir2pnt(dir) * dims - id;
            if (all(abs(diff_) < 1))
                acc = 1;
        }

        gDebugOutlier[id] = float4(acc, acc, acc, 1);
        if (acc == 1)
            gDebugPdf[id] = float4(0, 0, 0, 1);
        // gDebugPdf[id].r = min(acc - 1, gDebugPdf[id].r);
    }
    */
    /*
    gDebugTile.GetDimensions(dims.x, dims.y);

    if (all(id < dims))
    {
        int f = dims.x / 16;

        uint2 id_ = gParams.debugMinDim + id / f;
        float3 out = float3(0);

        uint reorder = gReorder[id_];
        uint2 offset = uint2(extractBits(reorder, 0, 4), extractBits(reorder, 4, 4));
        uint2 idx = gParams.debugMinDim + offset;
        Reservoir<Sample> res = unpackReservoirSample(gReservoir[idx], gReservoir2[idx]);
        reorder = encode2(res.x.direction, offset);

        if (false)
        {
            out = float(reorder & 0xFF) / 255.f;
        }
        else
        {
            out = color_map(float(reorder) / float(~0u));
        }
        gDebugTile[id] = float4(out, 1);
    */
    /*
    if (all(id < diff)) {
        if (gParams.parallaxCompensation) {
            VMF lobe = VMF(gVMM[gParams.debugMinDim + id], gVMM2[gParams.debugMinDim + id]);
            if (gParams.parallaxCompensation) lobe.reproject(gDebugPos[0].xyz);
            lobe.pack(gDebugVMM1[id], gDebugVMM2[id]);
        } else {
            gDebugVMM1[id] = gVMM[gParams.debugMinDim + id];
            gDebugVMM2[id] = gVMM2[gParams.debugMinDim + id];
        }
    }

    if (all(id < f * diff)) {
        uint2 id_ = gParams.debugMinDim + id / f;
        float3 out = float3(0);

        if (false) {
            VMF lobe = VMF(gVMM[id_], gVMM2[id_]);
            Reservoir<Sample> res = unpackReservoirSample(gReservoir[id_], gReservoir2[id_]);
            out = float3(res.W / res.M);
        }
        else if (false)
        {
            VMF lobe = VMF(gVMM[id_], gVMM2[id_]);
            if (gParams.parallaxCompensation) lobe.reproject(gDebugPos[0].xyz);
            float pdf = lobe.pdf(direction) / (diff.x * diff.y);
            out.r = isnan(pdf) && lobe.valid();
            out.g = (isnan(lobe.direction.x) || any(isnan(lobe.direction))) && lobe.valid();
            // out.g = !lobe.deepvalid() && lobe.valid();
            // out.b = !lobe.deepvalid2() && lobe.valid();
        } else if (true) {
            Reservoir<Sample> res = unpackReservoirSample(gReservoir[id_], gReservoir2[id_]);
            out = res.valid() && any(isnan(res.x.direction));
        }


        // out = lobe.kappa;
        // if (any(isnan(out))) out = float3(1.0, 0.0, 0.0);
        // else if (any(isinf(out))) out = float3(0.0, 1.0, 0.0);
        // else if (any(out > 1e9)) out = float3(0.0, 0.0, 1.0);
        // else if (any(out < 0.f)) out = float3(0.0, 0.0, 1.0);
        // else if (res.x.distance > 1e8) out = float3(1.0, 0.0, 0.0);
        // else if (!res.valid()) out = float3(1.0, 0.0, 0.0);
        // else if (abs(length(lobe.direction) - 1) > 1e-7) out = float3(0, 1, 0);
        // else if (isnan(lobe.kappa) || lobe.kappa == 0) out = float3(0, 1, 0);
        // else out = float3(0);
        gDebugPdf[id] = float4(out, 1);
        gDebugOut[id / f] = float4(out, 1);
    }
    */
    //}
}
