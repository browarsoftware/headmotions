/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package dtw;

/**
 *
 * @author Tomus
 */
public class EuclideanDistance implements IDistance{

    @Override
    public double calculate(double[] v1, double[] v2) {
        double res = 0;
        for (int a = 0; a < v1.length; a++)
            res += (v1[a] - v2[a]) * (v1[a] - v2[a]);
        return Math.sqrt(res);
    }
    
}
