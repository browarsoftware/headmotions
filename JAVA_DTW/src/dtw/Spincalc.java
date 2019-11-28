/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package dtw;

import Jama.EigenvalueDecomposition;
import Jama.Matrix;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 *
 * @author Tomus
 */
public class Spincalc {
    public static int ArgMin3(double a, double b, double c)
    {
      if (a<b)
      {
        if (a<c)
        {
          return 0;
        }
        else
        {
        return 2;
        }
      }
      else
      {
        if (b<c)
        {
          return 1;
        }
        else
        {
          return 2;
        }
      }
    }

    public static double dot_prod(double[]x, double[] y)
    {
        double dotProd = 0;
        for (int a = 0; a < x.length; a++)
            dotProd += x[a] * y[a];
        return dotProd;
    }
    /*
    public static double quat_similarity2(double[]x, double[] y)
    {
        double dot = dot_prod(x, y);
        if(dot < 0.0)
        {
            for (int a = 0; a < y.length; a++)
                y[a] *= -1;
            dot = dot_prod(x, y);
        }
        return (1 - dot);
    }*/
    
    public static double quat_similarity(double[]x, double[] y)
    {
        double dotProd = 0;
        for (int a = 0; a < x.length; a++)
            dotProd += x[a] * y[a];
        return (1 - Math.abs(dotProd));
    }
    
    
    public static double geo_norm(double []x, double[]y)
    {
        double[]aa = x;
        double[]bb = y;
        
        double d = aa[0]*aa[0] + aa[1]*aa[1] + aa[2]*aa[2] + aa[3]*aa[3];
        double []aa_inverse = {aa[0]/d, -aa[1]/d, -aa[2]/d, -aa[3]/d};
        
        double y0 = aa_inverse[0]*bb[0] - aa_inverse[1]*bb[1] - aa_inverse[2]*bb[2] - aa_inverse[3]*bb[3];
        double y1 = aa_inverse[0]*bb[1] + aa_inverse[1]*bb[0] + aa_inverse[2]*bb[3] - aa_inverse[3]*bb[2];
        double y2 = aa_inverse[0]*bb[2] - aa_inverse[1]*bb[3] + aa_inverse[2]*bb[0] + aa_inverse[3]*bb[1];
        double y3 = aa_inverse[0]*bb[3] + aa_inverse[1]*bb[2] - aa_inverse[2]*bb[1] + aa_inverse[3]*bb[0];
        
        double []times = {y0, y1, y2, y3};
        
        
        
        double norm_a = Math.sqrt(y0*y0 + y1*y1 +y2*y2 + y3*y3);
        double xx0 = Math.log(norm_a);
        double acos = Math.acos(y0 / norm_a);
        double norm_v = Math.sqrt((y1 * y1) + (y2 * y2) + (y3 * y3));
        double xx1 = y1 / norm_v * acos;
        double xx2 = y2 / norm_v * acos;
        double xx3 = y3 / norm_v * acos;
        
        
        
        return(Math.sqrt(xx0*xx0 + xx1*xx1 +xx2*xx2 + xx3*xx3));
    }
    
    public static double quat_similarity3(double []x, double[]y)
    {
        /*Quaternion a = new Quaternion(x[0], x[1], x[2],  x[3]);
        Quaternion b = new Quaternion(y[0], y[1], y[2],  y[3]);
        
        return Math.min(Quaternion.log(a.inverse().times(b)).norm(), Quaternion.log(b.inverse().times(a)).norm());*/
        return Math.min(geo_norm(x,y), geo_norm(y,x));
        //return geo_norm(x,y);
    }
    
    public static ArrayList DBA_one_iteration(ArrayList averageS, ArrayList sequences)
    {
        ArrayList tupleAssociation = new ArrayList();
        for (int t = 0; t < averageS.size(); t++)
            tupleAssociation.add(new ArrayList());
  
        int numberOfSignals = sequences.size();

        for (int k = 0; k < numberOfSignals; k++)
        {
            ArrayList sequence = (ArrayList)sequences.get(k);
            
            double [][]costMatrix = new double[averageS.size()][];
            int [][]pathMatrix = new int[averageS.size()][];
            for (int a = 0; a < costMatrix.length; a++)
            {
                costMatrix[a] = new double[sequence.size()];
                pathMatrix[a] = new int[sequence.size()];
            }
            
            
    
            costMatrix[0][0] = quat_similarity((double[])averageS.get(0),(double[])sequence.get(0));
                    
            pathMatrix[0][0] = -1;
    
            for (int i = 1; i < averageS.size(); i++)
            {
              costMatrix[i][0] = costMatrix[i-1][0] + quat_similarity((double[])averageS.get(i),(double[])sequence.get(0));
              pathMatrix[i][0] = 2;
            }
    
            for (int j = 1; j < sequence.size(); j++)
            {
              costMatrix[0][j] = costMatrix[0][j-1] + quat_similarity((double[])sequence.get(j),(double[])averageS.get(0));
              pathMatrix[0][j] = 1;
            }
    
            for (int i = 1; i < averageS.size(); i++)
            {
                for (int j = 1; j < sequence.size(); j++)
                {
                    int indiceRes = ArgMin3(costMatrix[i-1][j-1],costMatrix[i][j-1],costMatrix[i-1][j]);
                    pathMatrix[i][j] = indiceRes;

                    double res = 0;
                    if (indiceRes==0)
                    {
                      res = costMatrix[i-1][j-1];
                    }
                    else if (indiceRes==1)
                    {
                        res = costMatrix[i][j-1];
                    }
                    else if (indiceRes==2)
                    {
                        res = costMatrix[i-1][j];
                    }
                    costMatrix[i][j] = res + quat_similarity((double[])averageS.get(i),(double[])sequence.get(j));
                }
            }

            int i = averageS.size() - 1;
            int j = sequence.size() - 1;


            while(true)
            {
                ArrayList ttt = (ArrayList)tupleAssociation.get(i);
                double []ttt_helper = (double[])sequence.get(j);
                ttt.add(ttt_helper);
                if (pathMatrix[i][j]==0)
                {
                    i=i-1;
                    j=j-1;
                } else if (pathMatrix[i][j]==1)
                {
                    j=j-1;
                } else if (pathMatrix[i][j]==2)
                {
                    i=i-1;          
                } else
                {
                    break;
                }
            }
        }
  
        ArrayList averageSR = new ArrayList();
        for (int t = 0; t < averageS.size(); t++)
        {
            ArrayList df = (ArrayList)tupleAssociation.get(t);
            double []t_res = avg_quaternion_markley(df);
            averageSR.add(t_res);
        }
        return(averageSR);
    }

    public static ArrayList DBA_one_iteration(ArrayList averageS, ArrayList sequences, int index)
    {
        ArrayList tupleAssociation = new ArrayList();
        for (int t = 0; t < averageS.size(); t++)
            tupleAssociation.add(new ArrayList());
  
        int numberOfSignals = sequences.size();
        double norm_distance = 0;
        for (int k = 0; k < numberOfSignals; k++)
        {
            if (index != k)
            {
                ArrayList sequence = (ArrayList)sequences.get(k);

                double [][]costMatrix = new double[averageS.size()][];
                int [][]pathMatrix = new int[averageS.size()][];
                for (int a = 0; a < costMatrix.length; a++)
                {
                    costMatrix[a] = new double[sequence.size()];
                    pathMatrix[a] = new int[sequence.size()];
                }



                costMatrix[0][0] = quat_similarity((double[])averageS.get(0),(double[])sequence.get(0));

                pathMatrix[0][0] = -1;

                for (int i = 1; i < averageS.size(); i++)
                {
                  costMatrix[i][0] = costMatrix[i-1][0] + quat_similarity((double[])averageS.get(i),(double[])sequence.get(0));
                  pathMatrix[i][0] = 2;
                }

                for (int j = 1; j < sequence.size(); j++)
                {
                  costMatrix[0][j] = costMatrix[0][j-1] + quat_similarity((double[])sequence.get(j),(double[])averageS.get(0));
                  pathMatrix[0][j] = 1;
                }

                for (int i = 1; i < averageS.size(); i++)
                {
                    for (int j = 1; j < sequence.size(); j++)
                    {
                        int indiceRes = ArgMin3(costMatrix[i-1][j-1],costMatrix[i][j-1],costMatrix[i-1][j]);
                        pathMatrix[i][j] = indiceRes;

                        double res = 0;
                        if (indiceRes==0)
                        {
                          res = costMatrix[i-1][j-1];
                        }
                        else if (indiceRes==1)
                        {
                            res = costMatrix[i][j-1];
                        }
                        else if (indiceRes==2)
                        {
                            res = costMatrix[i-1][j];
                        }
                        costMatrix[i][j] = res + quat_similarity((double[])averageS.get(i),(double[])sequence.get(j));
                    }
                }

                int i = averageS.size() - 1;
                int j = sequence.size() - 1;


                while(true)
                {
                    ArrayList ttt = (ArrayList)tupleAssociation.get(i);
                    double []ttt_helper = (double[])sequence.get(j);
                    ttt.add(ttt_helper);
                    if (pathMatrix[i][j]==0)
                    {
                        i=i-1;
                        j=j-1;
                    } else if (pathMatrix[i][j]==1)
                    {
                        j=j-1;
                    } else if (pathMatrix[i][j]==2)
                    {
                        i=i-1;          
                    } else
                    {
                        break;
                    }
                }
                
                norm_distance = costMatrix[averageS.size() - 1][sequence.size() - 1] / (averageS.size() + sequence.size());
            }
        }
  
        ArrayList averageSR = new ArrayList();
        for (int t = 0; t < averageS.size(); t++)
        {
            ArrayList df = (ArrayList)tupleAssociation.get(t);
            double []t_res = avg_quaternion_markley(df);
            averageSR.add(t_res);
        }
        norm_distances.add(norm_distance);
        
        return(averageSR);
    }
    
    
    public static void testConvertion(List<Object> tt)
    {
        for (int a = 0; a < tt.size(); a++)
        {
            double[]ttt = (double[])tt.get(a);
            for (int b = 0; b < ttt.length; b++)
            {
                System.out.print(ttt[b]);
            }
            System.out.println();
        }
    }
    
    public static LinkedHashMap<String, Object> testConvertion2(List averageS, List sequence, String distance_type)
    {
        IDistance FUN = new EuclideanDistance();
        if (distance_type.compareTo("e") == 0)
            FUN = new EuclideanDistance();
        LinkedHashMap<String, Object> ref = myDTW(new EuclideanDistance(), new ArrayList(averageS), new ArrayList(sequence));
        //System.out.println(ref.get("normalized_distance"));
        return ref;
    }
    //tututut
    //my_dba <- function(sequences, iterationsCount = 100, index = -1, eps = 0.001)
    public static LinkedHashMap<String, Object> DBAtest(List sequences, int iterationsCount, int index, double eps)
    {
        /*ArrayList quat = new ArrayList();
        for (int a = 0; a < sequences.size(); a++)
        {
            ArrayList aa = ((MotionLog)sequences.get(a)).quaternion;
            quat.add(aa);
        }*/
        int[]si = new int[1];
        ArrayList avg = my_dba(new ArrayList(sequences), iterationsCount, index, eps, si);
        LinkedHashMap<String, Object> results = new LinkedHashMap<String, Object>();
        results.put("avg.signal", avg);
        results.put("avg.signal.id", si[0]);
        results.put("avg.signal.error", dba_error);
        return results;
    }
    
    
    //https://cran.r-project.org/web/packages/jdx/vignettes/Introduction.html
    public static LinkedHashMap<String, Object> myDTW(IDistance FUN, ArrayList averageS, ArrayList sequence)
    {
        ArrayList tupleAssociation = new ArrayList();
        for (int t = 0; t < averageS.size(); t++)
            tupleAssociation.add(new ArrayList());

        double [][]costMatrix = new double[averageS.size()][];
             int [][]pathMatrix = new int[averageS.size()][];
             for (int a = 0; a < costMatrix.length; a++)
             {
                     costMatrix[a] = new double[sequence.size()];
                     pathMatrix[a] = new int[sequence.size()];
             }

            ArrayList path1 = new ArrayList();
            ArrayList path2 = new ArrayList();
            //costMatrix[0][0] = quat_similarity((double[])averageS.get(0),(double[])sequence.get(0));
            costMatrix[0][0] = FUN.calculate((double[])averageS.get(0),(double[])sequence.get(0));

            pathMatrix[0][0] = -1;

            for (int i = 1; i < averageS.size(); i++)
            {
              //costMatrix[i][0] = costMatrix[i-1][0] + quat_similarity((double[])averageS.get(i),(double[])sequence.get(0));
                costMatrix[i][0] = costMatrix[i-1][0] + FUN.calculate((double[])averageS.get(i),(double[])sequence.get(0));
              pathMatrix[i][0] = 2;
            }

            for (int j = 1; j < sequence.size(); j++)
            {
              //costMatrix[0][j] = costMatrix[0][j-1] + quat_similarity((double[])sequence.get(j),(double[])averageS.get(0));
                costMatrix[0][j] = costMatrix[0][j-1] + FUN.calculate((double[])sequence.get(j),(double[])averageS.get(0));
              pathMatrix[0][j] = 1;
            }

            for (int i = 1; i < averageS.size(); i++)
            {
                for (int j = 1; j < sequence.size(); j++)
                {
                    int indiceRes = ArgMin3(costMatrix[i-1][j-1],costMatrix[i][j-1],costMatrix[i-1][j]);
                    pathMatrix[i][j] = indiceRes;

                    double res = 0;
                    if (indiceRes==0)
                    {
                      res = costMatrix[i-1][j-1];
                    }
                    else if (indiceRes==1)
                    {
                        res = costMatrix[i][j-1];
                    }
                    else if (indiceRes==2)
                    {
                        res = costMatrix[i-1][j];
                    }
                    //costMatrix[i][j] = res + quat_similarity((double[])averageS.get(i),(double[])sequence.get(j));
                    costMatrix[i][j] = res + FUN.calculate((double[])averageS.get(i),(double[])sequence.get(j));
                    
                }
            }

            int i = averageS.size() - 1;
            int j = sequence.size() - 1;


            while(true)
            {
                            path1.add(i);
                            path2.add(j);
                ArrayList ttt = (ArrayList)tupleAssociation.get(i);
                double []ttt_helper = (double[])sequence.get(j);
                ttt.add(ttt_helper);
                if (pathMatrix[i][j]==0)
                {
                    i=i-1;
                    j=j-1;
                } else if (pathMatrix[i][j]==1)
                {
                    j=j-1;
                } else if (pathMatrix[i][j]==2)
                {
                    i=i-1;          
                } else
                {
                    break;
                }
            }
                
            double norm_distance = costMatrix[averageS.size() - 1][sequence.size() - 1] / (averageS.size() + sequence.size());
            //https://cran.r-project.org/web/packages/jdx/vignettes/Introduction.html
            LinkedHashMap<String, Object> results = new LinkedHashMap<String, Object>();
            double []path1_array = new double[path1.size()];
            double []path2_array = new double[path2.size()];
            for (int a = 0; a < path1_array.length; a++)
            {
                path1_array[a] = (int)path1.get(path1_array.length - a - 1) + 1;
                path2_array[a] = (int)path2.get(path1_array.length - a - 1) + 1;
            }
            results.put("path1", path1_array);
            results.put("path2", path2_array);
            results.put("normalized_distance", norm_distance);
            return results;
    }
    
    static ArrayList norm_distances = null;
    
    public static double DTW_compare(ArrayList averageS, ArrayList sequence)
    {
        ArrayList tupleAssociation = new ArrayList();
        for (int t = 0; t < averageS.size(); t++)
            tupleAssociation.add(new ArrayList());
  
        double [][]costMatrix = new double[averageS.size()][];
        int [][]pathMatrix = new int[averageS.size()][];
        for (int a = 0; a < costMatrix.length; a++)
        {
            costMatrix[a] = new double[sequence.size()];
            pathMatrix[a] = new int[sequence.size()];
        }



        costMatrix[0][0] = quat_similarity((double[])averageS.get(0),(double[])sequence.get(0));

        pathMatrix[0][0] = -1;

        for (int i = 1; i < averageS.size(); i++)
        {
          costMatrix[i][0] = costMatrix[i-1][0] + quat_similarity((double[])averageS.get(i),(double[])sequence.get(0));
          pathMatrix[i][0] = 2;
        }

        for (int j = 1; j < sequence.size(); j++)
        {
          costMatrix[0][j] = costMatrix[0][j-1] + quat_similarity((double[])sequence.get(j),(double[])averageS.get(0));
          pathMatrix[0][j] = 1;
        }

        for (int i = 1; i < averageS.size(); i++)
        {
            for (int j = 1; j < sequence.size(); j++)
            {
                int indiceRes = ArgMin3(costMatrix[i-1][j-1],costMatrix[i][j-1],costMatrix[i-1][j]);
                pathMatrix[i][j] = indiceRes;

                double res = 0;
                if (indiceRes==0)
                {
                  res = costMatrix[i-1][j-1];
                }
                else if (indiceRes==1)
                {
                    res = costMatrix[i][j-1];
                }
                else if (indiceRes==2)
                {
                    res = costMatrix[i-1][j];
                }
                costMatrix[i][j] = res + quat_similarity((double[])averageS.get(i),(double[])sequence.get(j));
            }
        }

        int i = averageS.size() - 1;
        int j = sequence.size() - 1;


        while(true)
        {
            ArrayList ttt = (ArrayList)tupleAssociation.get(i);
            double []ttt_helper = (double[])sequence.get(j);
            ttt.add(ttt_helper);
            if (pathMatrix[i][j]==0)
            {
                i=i-1;
                j=j-1;
            } else if (pathMatrix[i][j]==1)
            {
                j=j-1;
            } else if (pathMatrix[i][j]==2)
            {
                i=i-1;          
            } else
            {
                break;
            }
        }
        
  
        ArrayList averageSR = new ArrayList();
        for (int t = 0; t < averageS.size(); t++)
        {
            ArrayList df = (ArrayList)tupleAssociation.get(t);
            double []t_res = avg_quaternion_markley(df);
            averageSR.add(t_res);
        }
        double dtwcost = costMatrix[averageS.size() - 1][sequence.size() - 1] / (averageS.size() + sequence.size());
        return(dtwcost);
    }

    
    public static Random random = new Random();
    /*public static ArrayList my_dba(ArrayList sequences, int iterationsCount, int index, double eps)
    {
        if (index < 0)
            index = random.nextInt(sequences.size());
        ArrayList average = (ArrayList)sequences.get(index);
        for (int a = 0; a < iterationsCount; a++)
        {
            System.out.print(a + " ");
            average = DBA_one_iteration(average,sequences);
        }
        return average;
    }*/

    public static MotionLog DBA(ArrayList sequences, int iterationsCount, int index, double eps)
    {
        ArrayList quat = new ArrayList();
        for (int a = 0; a < sequences.size(); a++)
        {
            ArrayList aa = ((MotionLog)sequences.get(a)).quaternion;
            quat.add(aa);
        }
        int[]si = new int[1];
        ArrayList avg = my_dba(quat, iterationsCount, index, eps, si);
        MotionLog mlml = (MotionLog)sequences.get(si[0]);
        MotionLog ml = new MotionLog(mlml.time, avg);
        return ml;
    }
    
    private static double[]dba_error = null;
    public static ArrayList my_dba(ArrayList sequences, int iterationsCount, int index, double eps, int[] selected_index)
    {
        norm_distances = new ArrayList();
        if (index < 0)
            index = random.nextInt(sequences.size());
        selected_index[0] = index;
        //ArrayList average = (ArrayList)sequences.get(index);
        
        ArrayList average = new ArrayList();
        ArrayList aa = (ArrayList)sequences.get(index);
        for (int a = 0; a < aa.size(); a++)
        {
            average.add(MotionLog.Copy((double[])aa.get(a)));
        }
        
        dba_error = new double[iterationsCount];
        for (int a = 0; a < iterationsCount; a++)
        {
            average = DBA_one_iteration(average, sequences, index);
            if (a == 0)
            {
                //System.out.println(a + " " + (double)norm_distances.get(a));
                dba_error[a] = (double)norm_distances.get(a);
            }
            else
            {
                //System.out.println(a + " " + Math.abs((double)norm_distances.get(a - 1) - (double)norm_distances.get(a)));
                dba_error[a] = Math.abs((double)norm_distances.get(a - 1) - (double)norm_distances.get(a));
            }
        }
        return average;
    }
    
    public static double[]euler2quaternion(double xR, double yR, double zR) throws Exception
    {
        //double[] a1 = c(zR, yR, xR) * (pi/180)
        return EA2Q(new double[]{zR * (Math.PI/180.0), yR * (Math.PI/180.0), xR * (Math.PI/180.0)});
    }
    
    public static double[] EA2Q(double[] EA) throws Exception
    {
        double theta = EA[1];
        if (Math.abs(theta) >= Math.PI/2.0)  
        { 
            throw new Exception("Second input Euler angle(s) outside -90 to 90 degree range");
        }
        //Half angles in radians
        double Hpsi = EA[0] / 2.0;
        double Htheta = EA[1] / 2.0;
        double Hphi = EA[2] / 2.0;
        
        //Pre-calculate cosines and sines of the half-angles for conversion.
        double c1 = Math.cos(Hpsi), c2 = Math.cos(Htheta), c3 = Math.cos(Hphi);
        double s1 = Math.sin(Hpsi), s2 = Math.sin(Htheta), s3 = Math.sin(Hphi);
        double c13 = Math.cos(Hpsi+Hphi), s13 = Math.sin(Hpsi+Hphi);
        double c1_3 = Math.cos(Hpsi-Hphi), s1_3 = Math.sin(Hpsi-Hphi);
        double c3_1 = Math.cos(Hphi-Hpsi), s3_1 = Math.sin(Hphi-Hpsi);
        
        return new double[]{c1*c2*c3+s1*s2*s3, c1*c2*s3-s1*s2*c3, c1*s2*c3+s1*c2*s3, s1*c2*c3-c1*s2*s3};
    }

    public static double[] quaternion2euler(double[]q)
    {
        double[] ea = Q2EA(q);
        return new double[]{ea[2] * (180.0/Math.PI), ea[1] * (180.0/Math.PI), ea[0] * (180.0/Math.PI)};
    }
    
    public static double[] Q2EA(double[] Q)
    {
        return new double[] {
            Math.atan2((2*(Q[1]*Q[2] + Q[0]*Q[3])),(Q[0]*Q[0] + Q[1]*Q[1] - Q[2]*Q[2] - Q[3]*Q[3])), 
            Math.atan2(-(2*(Q[1]*Q[3] - Q[0]*Q[2])),Math.sqrt(1-Math.pow(2*(Q[1]*Q[3] - Q[0]*Q[2]),2))),
            Math.atan2((2*(Q[2]*Q[3] + Q[0]*Q[1])),(Q[0]*Q[0] - Q[1]*Q[1] - Q[2]*Q[2] + Q[3]*Q[3]))};
    }
    
    //http://math.nist.gov/javanumerics/jama/
    public static double[] avg_quaternion_markley(double[][]Q)
    {
        double [][]A = {{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
        int M  = Q.length;
        Matrix Am = new Matrix(A);
        for (int a = 0; a < M; a++)
        {
            Matrix qm = new Matrix(Q[a], 1).transpose();
            Am = (qm.times(qm.transpose())).plus(Am);
        }
        EigenvalueDecomposition ed = new EigenvalueDecomposition(Am);
        Matrix eV = ed.getV();
        double[] er = ed.getRealEigenvalues();
        int maxIndex = 0;
        for (int a = 0; a < er.length; a++)
        {
            if (Math.abs(er[a]) > Math.abs(er[maxIndex]))
            {
                maxIndex = a;
            }
        }
        double[] avg = new double[4];
        for (int a = 0; a < 4; a++)
        {
            avg[a] = (eV.getArray())[a][maxIndex];
        }
        return avg;
    }
    
    public static double[] avg_quaternion_markley(ArrayList Q)
    {
        double [][]A = {{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
        int M  = Q.size();
        Matrix Am = new Matrix(A);
        for (int a = 0; a < M; a++)
        {
            Matrix qm = new Matrix((double[])Q.get(a), 1).transpose();
            Am = (qm.times(qm.transpose())).plus(Am);
        }
        EigenvalueDecomposition ed = new EigenvalueDecomposition(Am);
        Matrix eV = ed.getV();
        double[] er = ed.getRealEigenvalues();
        int maxIndex = 0;
        for (int a = 0; a < er.length; a++)
        {
            if (Math.abs(er[a]) > Math.abs(er[maxIndex]))
            {
                maxIndex = a;
            }
        }
        double[] avg = new double[4];
        for (int a = 0; a < 4; a++)
        {
            avg[a] = (eV.getArray())[a][maxIndex];
        }
        return avg;
    }
    
    public static double[] wavg_quaternion_markley(double[][]Q, double[]weights)
    {
        double [][]A = {{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
        int M  = Q.length;
        Matrix Am = new Matrix(A);
        double wSum = 0;
        for (int a = 0; a < M; a++)
        {
            Matrix qm = new Matrix(Q[a], 1).transpose();
            Am = (qm.times(qm.transpose())).times(weights[a]).plus(Am);
            wSum += weights[a];
        }
        Am.times(1.0 / wSum);
        EigenvalueDecomposition ed = new EigenvalueDecomposition(Am);
        Matrix eV = ed.getV();
        double[] er = ed.getRealEigenvalues();
        int maxIndex = 0;
        for (int a = 0; a < er.length; a++)
        {
            if (Math.abs(er[a]) > Math.abs(er[maxIndex]))
            {
                maxIndex = a;
            }
        }
        double[] avg = new double[4];
        for (int a = 0; a < 4; a++)
        {
            avg[a] = (eV.getArray())[a][maxIndex];
        }
        return avg;
    }

    public static ArrayList generateSignalEulerFromQuaternion(ArrayList ql)
    {
        ArrayList al = new ArrayList();
        for (Object q : ql) {
            al.add(quaternion2euler((double[])q));
        }
        return al;
    }
   
    
    public static ArrayList<String> listFilesForFolder(final File folder) {
        ArrayList<String> al = new ArrayList<String>();
        for (final File fileEntry : folder.listFiles()) {
            if (fileEntry.isDirectory()) {
                al.addAll(listFilesForFolder(fileEntry));
            } else {
                //System.out.println(fileEntry.getName());
                al.add(fileEntry.getName());
            }
        }
        return al;
    }
    
    public static ArrayList gaussianQuaternionSmoother(ArrayList quaternionSignal, int windowSize) throws Exception
    {
        ArrayList smoothedSignal = new ArrayList();
        int startInd = (int)Math.floor((double)windowSize / 2.0);
        int startLoop = startInd;
        int endInd = (int)Math.ceil((double)windowSize / 2.0);
        int endLoop = quaternionSignal.size() - endInd;
        
        double[]y = new double[]{0.02461245, 0.05423769, 0.09809816, 0.14562435, 0.17742735, 0.17742735, 0.14562435, 0.09809816, 0.05423769, 0.02461245};
        for (int a = 0; a < quaternionSignal.size(); a++)
        {
            double[]dd = (double[])quaternionSignal.get(a);
            double[]dd2 = new double[dd.length];
            for (int b = 0; b < dd.length; b++)
                dd2[b] = dd[b];
            smoothedSignal.add(dd2);
        }
        //ArrayList q = new ArrayList();
        double[][]q = null;
        q = new double[windowSize][];
        //for (int a = startLoop; a < endLoop; a++)
        for (int a = 0; a < quaternionSignal.size(); a++)
        {
            //q[a] = new double[4];
            for (int b = -startInd; b < endInd; b++)
            {
                double[] dd = null;
                if (a + b < 0)
                    dd = (double[])quaternionSignal.get(0);
                else if (a + b >= quaternionSignal.size())
                    dd = (double[])quaternionSignal.get(quaternionSignal.size() - 1);
                else
                    dd = (double[])quaternionSignal.get(a + b);
                q[b + startInd] = euler2quaternion(dd[0], dd[1], dd[2]);
            }
                //q.add(quaternionSignal.add(a + b));
            smoothedSignal.set(a, quaternion2euler(wavg_quaternion_markley(q, y)));
        }
        
        return smoothedSignal;
    }

    public static double[] Qnormalize(double[]q)
    {
        double[]qq = new double[q.length];
        double ss = 0;
        for (int a = 0; a < q.length; a++)
        {
            ss += q[a] * q[a];
        }
        ss = Math.sqrt(ss);
        for (int a = 0; a < q.length; a++)
        {
            qq[a] = q[a] / ss;
        }
        return qq;
    }
    
    public static double[] quaternion_multiplication(double[]Q1, double[]Q2)
    {
        //quaternion product
        double[]ab = new double[4];
        ab[0] = Q1[0]*Q2[0]-Q1[1]*Q2[1]-Q1[2]*Q2[2]-Q1[3]*Q2[3];
        ab[1] = Q1[0]*Q2[1]+Q1[1]*Q2[0]+Q1[2]*Q2[3]-Q1[3]*Q2[2];
        ab[2] = Q1[0]*Q2[2]-Q1[1]*Q2[3]+Q1[2]*Q2[0]+Q1[3]*Q2[1];
        ab[3] = Q1[0]*Q2[3]+Q1[1]*Q2[2]-Q1[2]*Q2[1]+Q1[3]*Q2[0];
        return(ab);
    }
    
    public static double[][] calculateAngles(double []signalX, 
                                            double []signalY, 
                                            double []signalZ, 
                                            double rX, 
                                            double rY, 
                                            double rZ) throws Exception
    {
        double []XX = new double[signalX.length];
        double []YY = new double[signalY.length];
        double []ZZ = new double[signalZ.length];
  
        for (int a = 0; a < XX.length; a++)
        {
            //a1 <- c(ZZ[a], YY[a], XX[a]) * (pi/180)
            //q <- EA2Q(a1,"zyx")
            double []q = euler2quaternion(signalX[a], signalY[a], signalZ[a]);
            q = Qnormalize(q);
            double []qR = euler2quaternion(rX, rY, rZ);
            //first q rotation than qR rotation - this is the right order of multiplication
            q = quaternion_multiplication(qR, q);
            double[]ea = quaternion2euler(q);

            XX[a] = ea[0];
            YY[a] = ea[1];
            ZZ[a] = ea[2];
        }
        double[][]ret = new double[3][];
        ret[0] = XX;
        ret[1] = YY;
        ret[2] = ZZ;
        return ret;
    }
    
    public static double []Qconj(double[]Q) 
    {
        //quaternion conjugate
        double[]q2 = new double[4];
        q2[0] = Q[0];
        q2[1] = -Q[1];
        q2[2] = -Q[2];
        q2[3] = -Q[3];
        return q2;
    }
    
    
    public static double[] vectQrot(double[] Q, double[]rr)
    {
        //Rotate a vector by a quaternion
        double[]Qr = quaternion_multiplication(quaternion_multiplication(Qconj(Q), new double[]{0,rr[0],rr[1],rr[2]}), Q);
        return (new double[]{Qr[1],Qr[2],Qr[3]});
    }
    
    public static double[] rotateVector(double[]vec, double xR, double yR, double zR) throws Exception
    {
        double[] vec1 = Qnormalize(vec);
        //double[] a1 = new double[]{zR * Math.PI / 180.0, yR * Math.PI / 180.0, xR * Math.PI / 180.0};
        double []q = euler2quaternion(xR, yR, zR);
        
        double[] vec_rot = vectQrot(q, vec1);
        return vec_rot;
    }
    
    public static double dotProduct(double[]vec1, double[]vec2)
    {
        double dot = 0;
        for (int a = 0; a < vec1.length; a++)
        {
            dot += vec1[a] * vec2[a];
        }
        return dot;
    }

    public static double vectorSimilarity(double[]vec1, double[]vec2)
    {
        return dotProduct(Qnormalize(vec1), Qnormalize(vec2));
    }

    public static MotionLog read_log_quaternion(String path)
    {
        return null;
    }
    
    public void aaa()
    {
        main(null);
    }
    
    public void RunDBA(int iterationsCount, int index, double eps, String outputFile,String []fileNames)
    {
        ArrayList<MotionLog> al = new ArrayList<MotionLog>();
        for (int a = 0; a < fileNames.length; a++)
        {
            MotionLog ml = new MotionLog(fileNames[a]);
            MotionLog aa = ml.rotateRecording(ml, (double[])ml.quaternion.get(0), 0, ml.quaternion.size(), false);
            al.add(aa);
        }
        
        MotionLog ml3 = DBA(al, iterationsCount, index, eps);
        ml3.Save(outputFile);
    }
    
    
    
    public void bbb()
    {

    }
    
    public Spincalc()
    {
        //main(null);
    }
    
    public static void main(String args[])
    {
        /*
        MotionLog m1 = new MotionLog("e:\\\\Publikacje\\\\headmotionclassification\\\\samplesT\\\\counterclockwise\\\\8.log");
        MotionLog m2 = new MotionLog("e:\\\\Publikacje\\\\headmotionclassification\\\\samplesT\\\\counterclockwise\\\\9.log");
        ArrayList m1l = new ArrayList();
        EuclideanDistance ed = new EuclideanDistance();
        LinkedHashMap<String, Object> res = myDTW(ed, m1.coord, m2.coord);
        int zzz = 0;
        zzz++;*/
        //MotionLog ml = new MotionLog("e:\\Publikacje\\headmotionclassification\\github\\samplesM\\clockwise\\65.log");
        //MotionLog ml2 = ml.rotateRecording(ml, (double[])ml.quaternion.get(0), 0, ml.quaternion.size(), false);
        
        /*
        String dir_path = "e:\\publikacje\\headmotionclassification\\samplesT\\";
        String []motions = {"clockwise", "counterclockwise",  "left", "node_head", "omega_left", "omega_right", "eight", "right"};

        String path_helper = dir_path + motions[0];
        ArrayList<String>fileNames = listFilesForFolder(new File(path_helper));
        ArrayList<MotionLog> al = new ArrayList<MotionLog>();
        
        
        
        for (int a = 0; a < fileNames.size(); a++)
        {
            MotionLog ml = new MotionLog(path_helper + "\\" + fileNames.get(a));
            MotionLog aa = ml.rotateRecording(ml, (double[])ml.quaternion.get(0), 0, ml.quaternion.size(), false);
            al.add(aa);
        }
        
        int iterationsCount = 3;
        int index = 0;
        double eps = 0.001;
        MotionLog ml3 = DBA(al, iterationsCount, index, eps);
        //ml3.NegateQuaternion();
        String save_path = "e:\\Publikacje\\headmotionclassification\\JAVA\\aa.log";
        ml3.Save(save_path);*/
        
        
        /*
        ArrayList quat = new ArrayList();
        for (int a = 0; a < al.size(); a++)
        {
            ArrayList aa = ((MotionLog)al.get(a)).quaternion;
            quat.add(aa);
        }
        int iterationsCount = 3;
        int index = 0;
        double eps = 0.001;
        ArrayList avg = my_dba(quat, iterationsCount, index, eps);
        */
        int a = 0;
        a++;
        a += 45;
    }
    /*
    public static Skeleton rotateChannel(Skeleton www1, boolean verbal) throws Exception
    {
        double yy = -1;
        double new_yy = 90;
        double correcting_yy = 0;
  
        double eps = 0.0001;
        for (int a = 0; a < 359; a++)
        {
            if (a < 90)
            {
                if (verbal) System.out.println("rot1 " + a);
 
                double[][] al = generateSignalEulear(www1, "Hips_R");
                double[][]rrr1 = calculateAngles(al[0], al[1], al[2], 0, a - eps, 0);
                double[]rrv = rotateVector(new double[]{1,0,0},rrr1[0][0], rrr1[1][0], rrr1[2][0]);
                new_yy = vectorSimilarity(rrv, new double[]{1,0,0});
                if (yy < new_yy)
               {
                    if (verbal) System.out.println(new_yy);
                    correcting_yy = a;
                    yy = new_yy;
                }
            }
            if (a >= 90 && a < 180)
            {
                if (verbal) System.out.println("rot1 " + (90 - eps) + " rot2 " + (a - 90));
                double[][] al = generateSignalEulear(www1, "Hips_R");
                double[][]rrr1 = calculateAngles(al[0], al[1], al[2], 0, 90 - eps, 0);
                rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, a - 90, 0);
                double[]rrv = rotateVector(new double[]{1,0,0},rrr1[0][0], rrr1[1][0], rrr1[2][0]);
                new_yy = vectorSimilarity(rrv, new double[]{1,0,0});
        
                if (yy < new_yy)
                {
                    if (verbal) System.out.println(new_yy);
                    correcting_yy = a;
                    yy = new_yy;
                }
            }
            if (a >= 180  && a < 270)
            {
                if (verbal) System.out.println("rot1 " + (90 - eps) + "rot2 " + (90 - eps) + " rot3 " + (a - 180));

                double[][] al = generateSignalEulear(www1, "Hips_R");
                double[][]rrr1 = calculateAngles(al[0], al[1], al[2], 0, 90 - eps, 0);
                rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, 90 - eps, 0);
                rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, a - 180, 0);
                double[]rrv = rotateVector(new double[]{1,0,0},rrr1[0][0], rrr1[1][0], rrr1[2][0]);
                new_yy = vectorSimilarity(rrv, new double[]{1,0,0});

                if (yy < new_yy)
                {
                    if (verbal) System.out.println(new_yy);
                    correcting_yy = a;
                    yy = new_yy;
                }
            }
            if (a >= 270)
            {
                if (verbal) System.out.println("rot1 " + (90 - eps) + "rot2 " + (90 - eps) + "rot3 " + (90 - eps) + " rot4 " + (a - 270));
                double[][] al = generateSignalEulear(www1, "Hips_R");
                double[][]rrr1 = calculateAngles(al[0], al[1], al[2], 0, 90 - eps, 0);
                rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, 90 - eps, 0);
                rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, 90 - eps, 0);
                rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, a - 270, 0);
        
                double[]rrv = rotateVector(new double[]{1,0,0},rrr1[0][0], rrr1[1][0], rrr1[2][0]);
                new_yy = vectorSimilarity(rrv, new double[]{1,0,0});
        
                if (yy < new_yy)
                {
                    if (verbal) System.out.println(new_yy);
                    correcting_yy = a;
                    yy = new_yy;
                }
            }
        }
  
        double[][]rrr1 = null;
        double[][] al = generateSignalEulear(www1, "Hips_R");
        //TUTUTUTU
        //correcting_yy = 300;
        if (correcting_yy < 90)
        {
            rrr1 = calculateAngles(al[0], al[1], al[2], 0, correcting_yy, 0);
        }
        if (correcting_yy >= 90 && correcting_yy < 180)
        {
            rrr1 = calculateAngles(al[0], al[1], al[2], 0, 90 - eps, 0);
            rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, correcting_yy - 90, 0);
        }
        if (correcting_yy >= 180 && correcting_yy < 270)
        {
            rrr1 = calculateAngles(al[0], al[1], al[2], 0, 90 - eps, 0);
            rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, 90 - eps, 0);
            rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, correcting_yy - 180, 0);
        }
        if (correcting_yy >= 270)
        {
            rrr1 = calculateAngles(al[0], al[1], al[2], 0, 90 - eps, 0);
            rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, 90 - eps, 0);
            rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, 90 - eps, 0);
            rrr1 = calculateAngles(rrr1[0], rrr1[1], rrr1[2], 0, correcting_yy - 270, 0);
        }
      
        double[]rrv = rotateVector(new double[]{1,0,0},rrr1[0][0], rrr1[1][0], rrr1[2][0]);
      
        Skeleton newSkeleton = Skeleton.CopySkeleton(www1, 0, www1.Time.length);
        
        int id = -1;
        for (int a = 0; a < newSkeleton.columnGropus.size(); a++)
        {
            //if (newSkeleton.columnGropus.get(a).columnName.toLowerCase().matches("Hips_R".toLowerCase()))
            if (newSkeleton.columnGropus.get(a).columnName.matches("Hips_R"))
            {
                id = a;
            }
        }
        int x = newSkeleton.columnGropus.get(id).columndId[0];
        int y = newSkeleton.columnGropus.get(id).columndId[1];
        int z = newSkeleton.columnGropus.get(id).columndId[2];
        
        
        newSkeleton.Features[x] = rrr1[0];
        newSkeleton.Features[y] = rrr1[1];
        newSkeleton.Features[z] = rrr1[2];
        return newSkeleton;
    }*/
/*
    public static void main4(String[]args)
    {
        String skeletonsPath = "e:\\mocap_data\\karate\\2016-07-13 Oyama KK\\evaluation\\mawashi_geri_l\\segmented";
        String outFile = "e:\\Publikacje\\java_dba_quaternion\\mawashi_geri_l";
        String bvhTemplate = "e:\\mocap_data\\karate\\2016-07-13 Oyama KK\\no_hands_and_legs.bvh";
        try {
            DBAAveraging(skeletonsPath,
                    outFile,
                    bvhTemplate,
                    1000, 
                    true,
                    false,
                    true);
            
             DBAAveraging(skeletonsPath, 
                    outFile + "_no_smoothing", 
                    bvhTemplate, 
                    1000, 
                    false, 
                    false,
                    true);
        } catch (Exception ex) {
            Logger.getLogger(Spincalc.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public static void main3(String[]args)
    {
        //Skeleton s = Skeleton.LoadSkeletonFromCSV("e:\\mocap_data\\karate\\2016-07-13 Oyama KK\\evaluation\\mae_geri_l\\segmented\\sample1.bvh.csv" );
        Skeleton s = Skeleton.LoadSkeletonFromCSV("e:\\mocap_data\\karate\\2017-06-09\\gotowe\\gotowe\\Heian Godan -2_3.bvh.csv" );
            try {
                s = rotateChannel(s, false);
                Skeleton.SaveSkeletonToCSV(s, "e:\\mocap_data\\karate\\2017-06-09\\gotowe\\gotowe\\sample1.bvh.csv");
                //Skeleton.SaveSkeletonToCSV(s, "e:\\Publikacje\\TrajectoryComparator\\sample1.bvh.csv");
                Skeleton.ExportCSVToBVH("e:\\mocap_data\\karate\\2017-06-09\\gotowe\\gotowe\\hg.bvh",
                    s,
                    //"e:\\Publikacje\\java_dba_quaternion\\mocap_data_avg\\no_hands_and_legs.bvh",
                    "e:\\mocap_data\\karate\\2017-06-09\\gotowe\\gotowe\\mp_no_hands.bvh",
                    false);
            } catch (Exception ex) {
                Logger.getLogger(Spincalc.class.getName()).log(Level.SEVERE, null, ex);
            }
    }
    
    */

    /*
    public static boolean igonreJoints(String str)
    {
        String[]arr = {"EndSite4", "EndSite8", "EndSite16", "EndSite24", "EndSite21", "LeftFoot", "RightFoot", "LeftHand", "RightHand"};
        for (int a = 0; a < arr.length; a++)
        {
            if (str.compareToIgnoreCase(arr[a]) == 0)
                return true;
        }
        return false;
    }
    
    public static void compareSignals(String skeletonsPath, String averagedSkeletonFile, boolean rotate, String outputFileName) throws Exception
    {
        ArrayList<String>fileNames = listFilesForFolder(new File(skeletonsPath));
        
        ArrayList<Skeleton> skeletons = new ArrayList<>();
        for (String fn : fileNames) {
            Skeleton s = Skeleton.LoadSkeletonFromCSV(skeletonsPath + "\\" + fn);
            if (rotate)
                s = rotateChannel(s, false);
            skeletons.add(s);
            //skeletons.add(Skeleton.LoadSkeletonFromCSV(skeletonsPath + "\\" + fn));
        }
        String[] jn = skeletons.get(0).JointsNames;
        ArrayList rr = new ArrayList();
        for (int a = 0; a < jn.length; a++)
        {
            if (!igonreJoints(jn[a]))
                rr.add(jn[a]);
        }
        jn = new String[rr.size()];
        for (int a = 0; a < rr.size(); a++)
        {
            jn[a] = (String)rr.get(a);
        }
        double[][] results = new double[skeletons.size()][];
        for (int a = 0; a < results.length; a++)
            results[a] = new double[jn.length];
        Skeleton averagedSkeleton = Skeleton.LoadSkeletonFromCSV(averagedSkeletonFile);
        averagedSkeleton = rotateChannel(averagedSkeleton, false);

        for (int a = 0; a < jn.length; a++)
        {
            String name = jn[a] + "_R";
            //name = "RightLeg_R";
            ArrayList quat = new ArrayList();
            for (Skeleton sk : skeletons) {
                quat.add(generateSignalQuaternionFromEulear(sk, name));
                
            }

            ArrayList signalAVG = generateSignalQuaternionFromEulear(averagedSkeleton, name);
            int zz = 0;
            for (Skeleton sk : skeletons) {
                //quat.add(generateSignalQuaternionFromEulear(sk, name));
                ArrayList signal = generateSignalQuaternionFromEulear(sk, name);
                double cmpResult = DTW_compare(signalAVG, signal);
                results[zz][a] = cmpResult;
                zz++;
            }
        }
        
        try (  PrintWriter out = new PrintWriter(outputFileName)  ){
            for (int a = 0; a < jn.length; a++)
            {
                if (a > 0) out.print(",");
                out.print(jn[a]);
            }
            out.println("");
            
            
            for (int a = 0; a < results.length; a++)
            {
                if (a > 0)
                    out.println("");
                for (int b = 0; b < results[a].length; b++)
                {
                    if (b > 0) out.print(",");
                    out.print(results[a][b]);
                }
            }
        }
    }*/
}
