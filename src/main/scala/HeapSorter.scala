import scala.reflect.ClassTag

/**
  * Created by yangshuxuan on 17-6-4.
  */
class HeapSorter[T:Ordering:ClassTag](val arr:Array[T]) {

  def maxHeapify(i:Int)(implicit heapSize:Int): Unit ={
    val left = i * 2 + 1
    val right = i * 2 + 2

    val largest1 =if(left<heapSize  && implicitly[Ordering[T]].gt(arr(left), arr(i))) left else i
    val largest2 = if (right<heapSize && implicitly[Ordering[T]].gt(arr(right),arr(largest1))) right else largest1
    if(largest2 != i){
      val t = arr(i)
      arr(i) = arr(largest2)
      arr(largest2) = t
      maxHeapify(largest2)
    }
  }
  def buildMaxHeap: Unit ={
    for(i<- (arr.size >> 1) - 1 to 0 by -1)
      maxHeapify(i)(arr.size)
  }
  def heapSort={
    buildMaxHeap
    for(i <- arr.size to 1 by -1){
      val t = arr(0)
      arr(0) = arr(i-1)
      arr(i-1) = t
      maxHeapify(0)(i-1)
    }
  }


}
object HeapSorter{
  def main(args: Array[String]): Unit = {
    //testMaxHeapify
    testHeapSort
  }
  def testMaxHeapify: Unit ={
    val k = Array(1,2,3)
    val p = new HeapSorter(k)
    p.maxHeapify(0)(k.size)
    val showMsg = k.mkString(",")
    println(showMsg)
  }
  def testHeapSort: Unit ={
    val k = Array(2,1,5,4,101,99,3)
    val p = new HeapSorter(k)
    p.heapSort
    val showMsg = k.mkString(",")
    println(showMsg)
  }
}
