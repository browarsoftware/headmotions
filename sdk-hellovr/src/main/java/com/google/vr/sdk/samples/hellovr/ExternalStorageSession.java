package com.google.vr.sdk.samples.hellovr;

import android.graphics.Bitmap;
import android.os.Environment;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;

public class ExternalStorageSession {
    public String LogFileName = null;
    public File PublicAlbumStorageDir = null;

    public ExternalStorageSession(File externalFile) throws IOException {
        PublicAlbumStorageDir = externalFile;
        boolean success = PublicAlbumStorageDir.mkdirs();
        this.LogFileName = externalFile.getName() + ".log";
        int a = 0;
        a++;
    }


    public void AppendLog(String type, float[] vector, long time)
    {
        String v = "";
        for (int a = 0; a < vector.length; a++) {
            if (a > 0)
                v += ",";
            v += vector[a];
        }
        AppendLog(type + ";" + time + ";" + v);
    }

    public void AppendLog(String type, String text, long time)
    {
        AppendLog(type + ";" + time + ";" + text);
    }

    public void AppendLog(String text)
    {
        File logFile = new File(PublicAlbumStorageDir.getAbsolutePath(),LogFileName);
        if (!logFile.exists())
        {
            try
            {
                logFile.createNewFile();
            }
            catch (IOException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        try
        {
            //BufferedWriter for performance, true to set append to file flag
            BufferedWriter buf = new BufferedWriter(new FileWriter(logFile, true));
            buf.append(text);
            buf.newLine();
            buf.close();
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }


}
