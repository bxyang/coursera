
import java.net.URL;
import java.net.HttpURLConnection;
import java.io.BufferedReader;
import java.io.InputStreamReader;


object Main {
    def main(args: Array[String]): Unit = {
    	//val obj = new MlfAssign1(new URL("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_15_train.dat"))
    	//print(obj.cyclic(500, 1))
    	//print(obj.multiRandom(500, 2000, 1))
    	//print(obj.multiRandomCyclic(500, 2000, 1))
    	
    	val obj = new MlfAssign1(new URL("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_train.dat"))
    	
    	val testData = obj.downloadData(new URL("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_test.dat"))
    	val errSum = (for (i <- 0 until 2000 ) yield obj.pocketPla(50, 1, i)).foldLeft(0.0)(
    			(sum: Double, w: List[Double]) => sum + obj.testOnData(w, testData))
    	//print(obj.testOnData(obj.mW.toList, testData))
    	println(errSum / 2000)
    }

}
