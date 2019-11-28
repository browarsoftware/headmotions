package com.google.vr.sdk.samples.hellovr;

import android.opengl.Matrix;

import com.google.vr.sdk.base.sensors.internal.Vector3d;

public class PathElement {
    public float[]TargetPosition = null;
    public float[]ModelTarget = null;
    public long CurrentTimeMillis = 0;
    public int TextureId = 0;
    public int curTargetObject = 0;
    public static  float scalingFactor = 0.25f;
    public PathElement(float[]TargetPosition, long CurrentTimeMillis, int TextureId, int curTargetObject)
    {
        this.TargetPosition = TargetPosition;
        this.CurrentTimeMillis = CurrentTimeMillis;
        this.TextureId = TextureId;
        this.curTargetObject = curTargetObject;

        float[]sm = new float[16];
        Matrix.setIdentityM(sm, 0);
        Matrix.scaleM(sm, 0, scalingFactor, scalingFactor, scalingFactor);

        ModelTarget = new float[16];

        Matrix.setIdentityM(ModelTarget, 0);
        Matrix.translateM(ModelTarget, 0, TargetPosition[0], TargetPosition[1], TargetPosition[2]);

        float[]mmmm = new float[16];

        Matrix.multiplyMM(mmmm, 0, ModelTarget, 0, sm, 0);
        ModelTarget = mmmm;
    }

    public PathElement(Quaternion quaternion, long CurrentTimeMillis, float pathDistance, int TextureId,
                       int curTargetObject) {
        Vector3d v3 = GenearateTargetPosition(quaternion, pathDistance);
        TargetPosition = new float[3];
        TargetPosition[0] = (float)v3.x;
        TargetPosition[1] = (float)v3.y;
        TargetPosition[2] = (float)v3.z;

        this.TextureId = TextureId;
        this.curTargetObject = curTargetObject;
        this.CurrentTimeMillis = CurrentTimeMillis;

        float[]sm = new float[16];
        Matrix.setIdentityM(sm, 0);
        Matrix.scaleM(sm, 0, scalingFactor, scalingFactor, scalingFactor);

        ModelTarget = new float[16];

        Matrix.setIdentityM(ModelTarget, 0);
        Matrix.translateM(ModelTarget, 0, TargetPosition[0], TargetPosition[1], TargetPosition[2]);

        float[]mmmm = new float[16];

        Matrix.multiplyMM(mmmm, 0, ModelTarget, 0, sm, 0);
        ModelTarget = mmmm;

        //Matrix.multiplyMM();

    }

    public static Vector3d GenearateTargetPosition(Quaternion hp, float pathDistance)
    {
        return hp.transform(new Vector3d(0,0,pathDistance));
    }
}
