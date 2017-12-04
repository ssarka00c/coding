package my.coding.practise

import scala.collection.mutable

/**
  * Created by subhankardeysarkar on 12/3/17.
  * https://leetcode.com/problems/insert-interval/description/
  *
  * Simple Idea, create list of all non overlapping intervals at start
  * while pushing the new interval check if it ovelaps with any interval..
  * if it does , move those intervals from the list to a new temp list
  * all the intervalsin the temp list will merge and become one interval , take min starttime and max endtime
  * push the new interval back into the main list with this new start and end times
  *
  */

case class Interval(start:Int,end:Int)
{
  override def hashCode(): Int = (start.toString + "," + end.toString).hashCode

  override def equals(obj: scala.Any): Boolean = {
    if(obj.isInstanceOf[Interval])
    {
      val intrvl = obj.asInstanceOf[Interval]
      if(intrvl.start == start && intrvl.end == end )
        true
      else
        false
    }
    else
      false
  }
}

object MergeInterval {

  var treeChilds = new collection.mutable.ListBuffer[Interval]()
  var tmpLst:List[Interval] = Nil


  def setNonOverlappingIntervals(lst:List[Interval]) =  { treeChilds ++= lst }

  def pushInterval(value:Interval) = {
    treeChilds.foreach { item =>
      if(checkOverlap(item,value))
        addToTempList(item)
    }
    treeChilds --= tmpLst
    val newInterval = getIntervalFromTemp
    treeChilds ++= List(newInterval)
    printIntervalList

  }

  def checkOverlap(v1:Interval,v2:Interval):Boolean = {
    v1 match {
      case c1 if(v1.end < v2.start) || (v1.start > v2.end) => false
      case _ => true
    }
  }

  def addToTempList(value:Interval) = {
    tmpLst ++= List(value)
  }

  def getIntervalFromTemp:Interval = {
    val start = tmpLst.map(_.start).min
    val end = tmpLst.map(_.end).max
    Interval(start,end)
  }

  def setIntervalToTree(value:Interval) = {treeChilds ++= List(value) }

  def printIntervalList = { println(treeChilds.toList.mkString(","))}



  def main(args: Array[String]): Unit = {

    val input = List((1,2),(3,5),(6,7),(8,10),(12,16)).map(x => Interval(x._1,x._2))
    setNonOverlappingIntervals(input)
    val insertInterval = List(Interval(4,9))
    insertInterval.foreach(pushInterval)


  }

}
