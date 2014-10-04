import java.net.URL;
import java.net.HttpURLConnection;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import scala.io.Source;
import scala.util.Random;

class MlfAssign1(dataUri: URL) {
	
	def downloadData(dataUri: URL) : List[List[scala.Double]] = {
		val src = Source.fromURL(dataUri);
		src.getLines.foldLeft(List.empty[List[scala.Double]])((data: List[List[scala.Double]], item: String) => 
			(for (field <- item.split("\t"); num <- field.split(" ")) yield java.lang.Double.parseDouble(num)).toList :: data)
	}
	
	def isMistaken(index: Int): Boolean = {
		val n = mW.size - 1
		var wx: Double = mW(n)
		for (i <- 0 until n) wx += mW(i) * mData(index)(i)
		if (wx == 0) mData(index)(n) > 0
		else wx * mData(index)(n) < 0
	}
	
	def isMistaken(index: Int, w: List[Double]): Boolean = {
		val n = w.size - 1
		var wx: Double = w(n)
		for (i <- 0 until n) wx += w(i) * mData(index)(i)
		if (wx == 0) mData(index)(n) > 0
		else wx * mData(index)(n) < 0
	}
	
	def isMistaken(w: List[Double], data: List[scala.Double]): Boolean = {
		val n = w.size - 1
		var wx: Double = w(n)
		for (i <- 0 until n) wx += w(i) * data(i)
		if (wx == 0) data(n) > 0
		else wx * data(n) < 0
	}
	
	def cyclic(maxIter: Int, factor: Double): Int = {
		var cnt: Int = 0
		var continueCnt: Int = 0
		var index: Int = 0
		while ((continueCnt < mTrainDataSize) && (cnt < maxIter)) {
			//print(mW)
			if (isMistaken(index)) {
				continueCnt = 0
				cnt += 1
				updateW(index, factor)
			} else {
				continueCnt += 1
			}
			index = (index + 1) % mTrainDataSize								
		}
		cnt
	}
	
	def randomCyclic(maxIter: Int, factor: Double, path: List[Int]): Int = {
		clearW()
		var cnt: Int = 0
		var continueCnt: Int = 0
		var index: Int = 0
		while ((continueCnt < mTrainDataSize) && (cnt < maxIter)) {
			//print(mW)
			if (isMistaken(path(index))) {
				continueCnt = 0
				cnt += 1
				updateW(path(index), factor)
			} else {
				continueCnt += 1
			}
			index = (index + 1) % mTrainDataSize								
		}
		//println(cnt)
		cnt		
	}
	
	def multiRandomCyclic(maxIter: Int, times: Int, factor: Double): Double = {
		val ret = (for (i <- 0 until times) yield randomCyclic(maxIter, factor, randomeShuffle(mData.size, i)))	
		ret.sum / (ret.size + 0.0)
	}
	def randomeShuffle(n: Int, seed: Int): List[Int] = {
		val arr: Array[Int] = (0 until n).toArray
		val rand: Random = new Random(seed)
		for (i <- 0 until n) {
			val idx = rand.nextInt(n-i)
			val mid = arr(idx)
			arr.update(idx, arr(n-i-1))
			arr.update(n-i-1, mid)
		}
		arr.toList		
	}
	
	def updateW(index: Int, factor: Double) = {
		val n = mW.size - 1
		val y = mData(index)(n) * factor
		mW.update(n, mW(n) + y)
		for (i <- 0 until n) mW.update(i, mW(i) +  y*mData(index)(i))
	}
	
	def tryUpdateW(index: Int, factor: Double): List[Double] = {
		val n = mW.size - 1
		val y = mData(index)(n) * factor
		(mW(n) + y)::(for (i <- 0 until n) yield mW(i) +  y*mData(index)(i)).toList.asInstanceOf[List[Double]]
	}
	
	def clearW() = {
		for (i <- 0 until mW.size) mW.update(i, 0)
	}
	
	def pocketPla(maxIter: Int, factor: Double, seed: Int): List[Double] = {
		clearW()
		var lastMistakeSize = mTrainDataSize
		val rand = new Random(seed)
		for (i <- 0 until maxIter) {
			val mistakenData = for (i <- 0 until mTrainDataSize; if (isMistaken(i))) yield i
			val index = mistakenData(rand.nextInt(mistakenData.size))
			val w = tryUpdateW(index, factor)
			var cnt = 0
			for (i <- 0 until mTrainDataSize) if (isMistaken(i, w)) cnt+=1
			if (cnt < lastMistakeSize) {
				lastMistakeSize = cnt
				mW = w.toArray
				//print("update!")
			}
			
			//if (i == maxIter - 1) mW = w.toArray
		}
		count+=1
		if (count % 100 == 0) println(count)
		mW.toList
	}
	
	def testOnData(w: List[Double], testData: List[List[scala.Double]]): Double = {
		var errCnt = 0
		for (i <- 0 until testData.size) {
			if (isMistaken(w, testData(i))) errCnt += 1
		}
		//println(errCnt / (testData.size + 0.0))
		errCnt / (testData.size + 0.0)
	}
	
	val mDataUrl: URL = dataUri
	val mData: List[List[scala.Double]] = downloadData(mDataUrl)
	val mTrainDataSize: Int = mData.size
	var mW: Array[Double] = Array(0,0,0,0,0)
	var count: Int = 0
}
