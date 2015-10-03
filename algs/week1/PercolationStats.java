import java.util.Random;
public class PercolationStats {
    public PercolationStats(int N, int T) {     // perform T independent experiments on an N-by-N grid
        _sites_ht = N;
        _t = T;
        _p = new double[N];
        for (int i = 0; i < T; i++) {
            Percolation perc = new Percolation(N);
            Random r = new Random();
            while (!perc.percolates()) {
                int p = r.nextInt(N) + 1;
                int q = r.nextInt(N) + 1;
                perc.open(p, q);
            }
            _p[i] = perc.siteFilledPerc();           
        }
    }

    public double mean() {                     // sample mean of percolation threshold
        double sum = 0;
        for (int i = 0; i < _t; i++)
            sum += _p[i];
        return sum/_t;
    }

    private int _sites_ht;
    private int _t;
    private double _p[];

    public static void main(String[] args) {  // test client (described below)
        int n = Integer.parseInt(args[0]);
        int t = Integer.parseInt(args[1]);
        PercolationStats pers = new PercolationStats(n, t);
        System.out.println(pers.mean());
    }
}
