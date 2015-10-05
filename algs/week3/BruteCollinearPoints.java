import java.lang.IllegalArgumentException;
import java.lang.NullPointerException;
import java.util.ArrayList;
import java.util.Arrays;

public class BruteCollinearPoints {
    public BruteCollinearPoints(Point[] points) {   // finds all line segments containing 4 points
        int n = points.length;
        if (n == 0) throw new java.lang.NullPointerException();
        for (int i = 0; i < n; i++) {
            if (points[i] == null) throw new java.lang.NullPointerException();
        }
        Arrays.sort(points);
        for (int i = 1; i < n; i++) {
            if (points[i].compareTo(points[i-1]) == 0)
                throw new java.lang.IllegalArgumentException();
        }
        
        nPointsLine(points, 4);
    }
    
    public int numberOfSegments() {       // the number of line segments
        return segs.size();
    }
    
    public LineSegment[] segments() {               // the line segments
        return segs.toArray(new LineSegment[segs.size()]);
    }
    
    private boolean isLine(Point[] segp) {
        int n = segp.length;
        if (n < 2) return false;
        double slp = segp[0].slopeTo(segp[1]);
        for (int i = 2; i < n; i++) {
            if (Math.abs(segp[0].slopeTo(segp[i]) - slp) > EPS) return false; 
        }
        return true;
    }

    private int min(Point[] segp) {
        int n = segp.length;
        int r = 0;
        for (int i = 1; i < n; i++) {
            if (segp[r].compareTo(segp[i]) > 0) r = i;
        }
        return r;
    }

    private int max(Point[] segp) {
        int n = segp.length;
        int r = 0;
        for (int i = 1; i < n; i++) {
            if (segp[r].compareTo(segp[i]) < 0) r = i;
        }
        return r;
    }

    private void nPointsLine(Point[] points, int n){
        Point[] subp = new Point[n];
        subset_core(points, n, 0, subp, 0);
    }
    
    private void subset_core(Point[] points, int n, int cur_pos, Point[] subp, int cnt) {
        if ((cur_pos == points.length) || (cnt == n)) {
            if (cnt != n) return;
            //System.out.println("4-size subset");
            if (!isLine(subp)) return;
            segs.add(new LineSegment(subp[min(subp)], subp[max(subp)]));
            return;
        }
        
        subset_core(points, n, cur_pos + 1, subp, cnt);
        subp[cnt] = points[cur_pos];
        subset_core(points, n, cur_pos + 1, subp, cnt + 1);
    }

    public final double EPS = 0.0000001;
    public LineSegment[] segments;
    private ArrayList<LineSegment> segs = new ArrayList<LineSegment>();
}
