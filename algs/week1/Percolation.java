import java.util.Arrays;

public class Percolation {
    public Percolation(int N) {             // create N-by-N grid, with all sites blocked
        if (N <=0 )
            throw new java.lang.IllegalArgumentException();
        _sites_ht = N;
        _sites_num = N * N;
        _sites = new int[_sites_num + 1];
        _cnts = new int[_sites_num + 1];
        for (int i = 1; i <= _sites_num; i++) {
            _sites[i] = i;
            _cnts[i] = 0;
        }
    }

    public void open(int i, int j) {          // open site (row i, column j) if it is not open already
        indexOutOfBoundsCheck(i, j);
        int idx = point2idx(i, j);
        if (_cnts[idx] != 0) return; 
        _cnts[idx] = 1;
        if ( (j > 1) && isOpen(i, j - 1)) union_site(idx, point2idx(i, j - 1));
        if ( (j + 1 <= _sites_ht) && isOpen(i, j + 1)) union_site(idx, point2idx(i, j + 1));
        if ( (i > 1) && isOpen(i - 1, j)) union_site(idx, point2idx(i - 1, j));
        if ( (i + 1 <= _sites_ht) && isOpen(i + 1, j)) union_site(idx, point2idx(i + 1, j));
    }

    public boolean isOpen(int i, int j) {    // is site (row i, column j) open?
        indexOutOfBoundsCheck(i, j);
        return (_cnts[point2idx(i, j)] != 0);
    }
   
    public boolean isFull(int i, int j) {    // is site (row i, column j) full?
        if (!isOpen(i, j)) return false;
        int idx = point2idx(i, j);
        for (int col = 1; col <= _sites_ht; col++) {
            if (isOpen(1, col) && connected(col, idx)) return true;
        }
        return false;
    }

    private boolean connected(int i, int j) {
        return root(i) == root(j);
    }

    private void union_site(int i, int j) {
        int ri = root(i);
        int rj = root(j);
        if (ri == rj) return;
        
        if (_cnts[ri] > _cnts[rj]) {
            _sites[rj] = ri;
            _cnts[ri] += _cnts[rj];
        } else {
            _sites[ri] = rj;
            _cnts[rj] += _cnts[ri];
        }
    }

    private int root(int i) {
        while (i != _sites[i]) {
            _sites[i] = _sites[_sites[i]];
            i = _sites[i];
        }
        return i;
    }

    private void indexOutOfBoundsCheck(int i, int j) {
        if ((i < 1) || (i > _sites_ht) || (j < 1) || (j > _sites_ht))
            throw new java.lang.IndexOutOfBoundsException();
    }

    private int point2idx(int i, int j) {
        return (i-1)*_sites_ht + j;
    }

    public boolean percolates() {            // does the system percolate?
        for (int col = 1; col < _sites_ht; col++) {
            if (isFull(_sites_ht, col)) return true;
        }
        return false;
    }

    public double siteFilledPerc() {
        double t = 0;
        for (int i = 1; i <= _sites_num; i++)
            if (_cnts[i] != 0) t++;
        return t/_sites_num;
    }
    
    private int _sites[];
    private int _sites_ht;
    private int _sites_num;
    private int _cnts[];
   //public static void main(String[] args)  // test client (optional)
}
