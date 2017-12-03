package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/3/17.
  */


case class Queue(n:Int)
{

  var arr = Array.ofDim[Int](n)
  var numItems = 0
  var currentAverage = 0
  var previousNumItems = 1

  def enqueue(num:Int) = {

    var dequedItem = 0

    if(numItems == n)
      {
        dequedItem = dequeue()
      }
    arr(numItems) = num
    numItems +=1

    currentAverage = ((currentAverage * previousNumItems) - dequedItem + num)/numItems
    previousNumItems = numItems
    println("Inserted Number = " + num + ", Moving Average = " + currentAverage + ", Dropped = " + dequedItem + " Queue = " + arr.mkString(","))

  }

  def dequeue():Int = {

    if(numItems == 0)
      return -1

    val itemPopped = arr(0)
    numItems -=1
    shiftLeft
    itemPopped

  }

  def shiftLeft = {
    for(i<- 0 until numItems )
      arr(i) = arr(i+1)
  }


}

object RunningAverageWithQueue {

  def main(args: Array[String]): Unit = {

    val myQueue = Queue(5)
    var i =1
    val r = scala.util.Random
    while(i<100) {
      myQueue.enqueue(r.nextInt(10000))
      i+=1
    }

  }

}
