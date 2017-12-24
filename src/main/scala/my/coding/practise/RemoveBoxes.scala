package my.coding.practise

import scala.collection.mutable

/**
  * Created by subhankardeysarkar on 12/11/17.
  * https://leetcode.com/problems/remove-boxes/description/
  * Remove Most Frequest Boxes to get Maximum Iteration,
  * Empty all boxes by multiple Iterations
  */
object RemoveBoxes extends App{

  var inputArray = Array(1, 3, 2, 2, 2, 3, 4, 3, 1)
  val hm = new mutable.HashMap[Int,Int]()
  var points = 0

  while (inputArray.length > 0)
    {
      val maxs = getMostFrequentContinueousElemnts(inputArray)
      points += maxs._2 * maxs._2
      inputArray = inputArray.zipWithIndex.filter( x=> !((maxs._1 to (maxs._1 + maxs._2) ).contains(x._2))).map(_._1)
    }

  println(s"Maximum Points = ${points}")


  def getMostFrequentContinueousElemnts(arr:Array[Int]):(Int,Int) = {     // index of max Item and length

    hm.clear()
    var tArr:Array[Int] = Array()
    var curItem =arr(0)
    var curItemCount = 1
    var maxItemIndex = 0
    var maxItemCount = 0
    for(i<- 1 until arr.length -1)
      {
        if(curItem == arr(i))
          curItemCount +=1
        else
          {
          //  hm.put(i-curItemCount,curItemCount)  // save the item startIndex and Count
            if(curItemCount > maxItemCount)
              {
                maxItemCount = curItemCount
                maxItemIndex = i - curItemCount
              }
            curItem = arr(i)
            curItemCount = 1
          }

      }
    (maxItemIndex,maxItemCount)
  }




}
