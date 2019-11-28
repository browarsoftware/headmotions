/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package dtw;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Tomus
 */
public class MotionLog {
    long[]time = null;
    ArrayList quaternion = new ArrayList();
    ArrayList coord = new ArrayList();
    ArrayList euler_angles = new ArrayList();
    
    double[] mulleft(double[] one, double[] other)
    {
        double newX = other[3] * one[0] + other[0] * one[3] + other[1] * one[2] - other[2] * one[1];
        double newY = other[3] * one[1] + other[1] * one[3] + other[2] * one[0] - other[0] * one[2];
        double newZ = other[3] * one[2] + other[2] * one[3] + other[0] * one[1] - other[1] * one[0];
        double newW = other[3] * one[3] - other[0] * one[0] - other[1] * one[1] - other[2] * one[2];
        double[] res = {newX,newY,newZ,newW};
        return res;
    }
    
    double[] transform (double x,double y,double z,double w,double vx,double vy,double vz)
    {
      double[]tmp2 = {-x, -y, -z, w};
      double[]tmp1 = {vx, vy, vz, 0};
      double[]rr = mulleft(mulleft(tmp2, tmp1),new double[]{x,y,z,w});
      double[] res = {rr[0],rr[1],rr[2]};
      return res;
    }

    public MotionLog()
    {
    }
    
    public MotionLog(long[]time, ArrayList quaternions)
    {
        this.time = Copy(time);
        double[]qq = null;
        for (int a = 0; a < quaternions.size(); a++)
        {
            qq = Copy((double[])quaternions.get(a));
            this.quaternion.add(qq);
            double[] ea = {getPitchRad(qq), getYawRad(qq), getRollRad(qq)};
            this.euler_angles.add(ea);
            double[] vec = {0,0,-1};
            double[] Vrot = transform(qq[0], qq[1], qq[2], qq[3],
                        vec[0], vec[1], vec[2]);
            coord.add(Vrot);
        }
    }
    
    public void NegateQuaternion()
    {
        double[]qq = null;
        double[]ea = null;
        double[]coo = null;
        double qHelp = 0;
        for (int a = 0; a < quaternion.size(); a++)
        {
            qq = (double[])quaternion.get(a);
            for (int b = 0; b < qq.length; b++)
                qq[b] *= -1;
            
            //this.quaternion.add(qq);
            ea = (double[])euler_angles.get(a);
            ea[0] = getPitchRad(qq);
            ea[1] = getYawRad(qq);
            ea[2] = getRollRad(qq);

            double[] vec = {0,0,-1};
            double[] Vrot = transform(qq[0], qq[1], qq[2], qq[3],
                        vec[0], vec[1], vec[2]);
            
            coo = (double[])coord.get(a);
            coo[0] = Vrot[0];
            coo[1] = Vrot[1];
            coo[2] = Vrot[2];
        }
    }
    
    public void Save(String path)
    {
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(path));
            double[] ar = null;
            for (int a = 0; a < time.length; a++)
            {
                ar = (double[])quaternion.get(a);
                writer.write("q;" + time[a] + ";" + ar[0] + ","
                + ar[1] + "," + ar[2] + "," + ar[3] + "\r\n");
                ar = (double[])euler_angles.get(a);
                writer.write("v;" + time[a] + ";" + ar[0] + ","
                + ar[1] + "," + ar[2] + "\r\n");
            }
            writer.close();
        } catch (IOException ex) {
            Logger.getLogger(MotionLog.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    
    public MotionLog(String path)
    {
        ArrayList time_help = new ArrayList();
        double []vec = {0, 0, -1};
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            String line;
            
            //skeleton = [];
            while ((line = br.readLine()) != null) {
               // process the line.
               String[] splitStr = line.split(";");
               if (splitStr[0].compareToIgnoreCase("q") == 0)
               {
                   time_help.add(Long.parseLong(splitStr[1]));
                   String[] splitStr2 = splitStr[2].split(",");
                   double[]quat = new double[splitStr2.length];
                   for (int a = 0; a < quat.length; a++)
                   {
                       quat[a] = Double.parseDouble(splitStr2[a]);
                   }
                   quaternion.add(quat);
                   
                   double[]Vrot = transform(quat[0], quat[1], quat[2], quat[3],
                      vec[0], vec[1], vec[2]);
                   coord.add(Vrot);
               }
               if (splitStr[0].compareToIgnoreCase("v") == 0)
               {
                   String[] splitStr2 = splitStr[2].split(",");
                   double[]quat = new double[splitStr2.length];
                   for (int a = 0; a < quat.length; a++)
                   {
                       quat[a] = Double.parseDouble(splitStr2[a]);
                   }
                   euler_angles.add(quat);
               }
            }
        } catch (FileNotFoundException ex) {
            Logger.getLogger(MotionLog.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(MotionLog.class.getName()).log(Level.SEVERE, null, ex);
        }
        time = new long[time_help.size()];
        for (int a = 0; a < time_help.size(); a++)
           time[a] = (long)time_help.get(a);
        //MotionLog
    }
    
    public static double[] Copy(double[] ar)
    {
        double dd[] = new double[ar.length];
        for (int a = 0; a < ar.length; a++)
        {
            dd[a] = ar[a];
        }
        return dd;
    }
    
    public static long[] Copy(long[] ar)
    {
        long dd[] = new long[ar.length];
        for (int a = 0; a < ar.length; a++)
        {
            dd[a] = ar[a];
        }
        return dd;
    }
    
    public MotionLog Copy()
    {
        MotionLog ml = new MotionLog();
        for (int a = 0; a < this.coord.size(); a++)
        {
            //double[]dd = (double[])this.coord.get(a);
            ml.coord.add(Copy((double[])this.coord.get(a)));
            ml.euler_angles.add(Copy((double[])this.euler_angles.get(a)));
            ml.quaternion.add(Copy((double[])this.quaternion.get(a)));
        }
        ml.time = Copy(this.time);
        return ml;
    }
    
    public double[] inverse(double[]q)
    {
        double sum = 0;
        for (int a = 0; a < q.length; a++)
            sum += q[a] * q[a];
        double[]qq = {-q[0] / sum, -q[1] / sum, -q[2] / sum, q[3] / sum};
        return qq;
    }
    
    public double getGimbalPole(double []q)
    {
        double t = q[1] * q[0] + q[2] * q[3];
        if (t > 0.499)
            return 1;
        if (t < -0.499)
            return -1;
        return 0;
    }

    public double getRollRad(double[]q)
    {
        double pole = getGimbalPole(q);
        if (pole == 0)
        {
            return (Math.atan2(2.0 * (q[3] * q[2] + q[1] * q[0]), 1.0 - 2.0 * (q[0] * q[0] + q[2] * q[2])));
        } else
        {
            return (pole * 2.0 * Math.atan2(q[2], q[3]));
        }
    }

    public double clamp(double value, double min, double max) 
    {
        if (value < min) return (min);
        if (value > max) return (max);
        return (value);
    }

    public double getPitchRad(double[]q)
    {
        double pole = getGimbalPole(q);
        if (pole == 0)
        {
            return(Math.asin(clamp(2.0 * (q[3] * q[0] - q[2] * q[1]), -1.0, 1.0)));
        }
        else
        {
            return (pole * Math.PI * 0.5);
        }
    }

    public double getYawRad(double[]q)
    {
        if (getGimbalPole(q) == 0)
        {
            return (Math.atan2(2.0 * (q[1] * q[3] + q[0] * q[2]), 1.0 - 2.0 * (q[1] * q[1] + q[0] * q[0])));
        } else
        {
            return (0);
        }
    }
    
    public MotionLog rotateRecording(MotionLog signal, double[] quat, int from, int to, boolean only_euler_angles)
    {
        MotionLog signal_helper = signal.Copy();
        signal_helper.euler_angles.clear();
        if (!only_euler_angles)
        {
            signal_helper.quaternion.clear();
            signal_helper.coord.clear();
        }
        for (int a = from; a < to; a++)
        {
            double[] qq = mulleft(inverse(quat), (double[])signal.quaternion.get(a));
            double[] ea = {getPitchRad(qq), getYawRad(qq), getRollRad(qq)};
            signal_helper.euler_angles.add(ea);
            if(!only_euler_angles)
            {
                double[] vec = {0,0,-1};
                double[] Vrot = transform(qq[0], qq[1], qq[2], qq[3],
                        vec[0], vec[1], vec[2]);
                signal_helper.quaternion.add(qq);
                signal_helper.coord.add(Vrot);
            }
        }
        return signal_helper;
    }
}
