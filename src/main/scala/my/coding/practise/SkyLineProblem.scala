package my.coding.practise

/**
  * Created by subhankardeysarkar on 1/7/18.
  * https://leetcode.com/problems/the-skyline-problem/description/
  */
object SkyLineProblem {

  case class Interval(start:Int,end:Int,height:Int)
  var overlappedInterval:List[Interval] = Nil

  def addInterval(nvl:Interval) = {

  }

  def publishSkyLine(lst:List[Interval]) = {

  }

  def getMax(a:Int,b:Int): Int =
  {
    if(a>b)
      a
    else
      b
  }

  def containsInterval(interval:Interval, checkInterval:Interval): Boolean = {
    if(checkInterval.start >= interval.start && checkInterval.end <= interval.end)
      true
    else
      false
  }

  def intervalOnLeftWithOverlap(interval:Interval, checkInterval:Interval) : Boolean = {

    if(checkInterval.start < interval.start && checkInterval.end > interval.start && checkInterval.end<interval.end)
      true
    else
      false
  }

  def splitIntervals(v1:Interval, v2:Interval):List[Interval] = {

    var retList:List[Interval] = Nil
    if(hasOverlap(v1,v2))
      {
        if(containsInterval(v1,v2))
          {
            if(v2.height > v1.height) {
              val f1 = v2.start - v1.start
              val f2 = v2.end - v2.start
              val f3 = v1.end - v2.end

              if(f1 > 0)
                retList ::= Interval(v1.start,v2.start,v1.height)
              if(f2>0)
                retList ::= v2
              if(f3 > 0)
                retList ::= Interval(v2.end,v1.end,v1.height)
            }

            return retList
          }

        if(containsInterval(v2,v1))
          {

            if(v1.height > v2.height) {
              val f1 = v1.start - v2.start
              val f2 = v1.end - v1.start
              val f3 = v2.end - v1.end

              if(f1 > 0)
                retList ::= Interval(v2.start,v1.start,v2.height)
              if(f2>0)
                retList ::= v1
              if(f3 > 0)
                retList ::= Interval(v1.end,v2.end,v2.height)
            }

            return retList
          }

        if(intervalOnLeftWithOverlap(v2,v1))
          {
            //v1 is on left of v2 with overlap
            if(v1.height > v2.height)
              {
                retList ::= v1
                retList ::= Interval(v1.end,v2.end,v2.height)
              }
            else
              {
                retList ::= Interval(v1.start,v2.start,v1.height)
                retList ::= v2
              }

            return retList
          }
        else
          {
            // v1 is on right with overlap
            if(v2.height > v1.height)
            {
              retList ::= v2
              retList ::= Interval(v2.end,v1.end,v1.height)
            }
            else
            {
              retList ::= Interval(v2.start,v1.start,v2.height)
              retList ::= v1
            }

            return retList
          }



      }
    else
     Nil
  }

  def hasOverlap(v1:Interval,v2:Interval):Boolean = {

    if(v1.end < v2.start || v2.end < v1.start)
      false
    else
      true

  }

  def main(args: Array[String]): Unit = {

    var tList:List[Interval] = Nil
    val input = List(
      Interval(2,9,10), Interval(3,7,15), Interval(5,12,12), Interval(15,20,10), Interval(19,24,8)
    ).sortBy(_.start)

    for(e <- input)
      {
        if(overlappedInterval.isEmpty)
          overlappedInterval ::=e
        else
          {

            var oneOverLap=false
            tList = Nil
            for(j <- overlappedInterval) {
              if(hasOverlap(j,e))
                {
                  tList ++= splitIntervals(j,e)
                  oneOverLap = true
                }
              else
                tList ::= j

            }

            if(!oneOverLap)
              {
                tList ::= e
              }

            overlappedInterval = tList
            //println(overlappedInterval)
          }
      }


    val connectedList = overlappedInterval.sortBy(_.start)
    println(connectedList)

    var output:List[(Int,Int)] = Nil
    for(i <- connectedList.indices) {

      if (i == 0) {
        output ::= (connectedList(i).start, connectedList(i).height)
      }
      else {

        var curItem = connectedList(i)
        var pItem = connectedList(i-1)

        if(curItem.start == pItem.end)
          {
            output ::= ( curItem.start, curItem.height )
          }
        else
          {
            //end previous item
            output ::= (pItem.end,0)
            output ::= (curItem.start,curItem.height)
          }


      }
    }

    //at the end just the last item on the list
    output ::= (connectedList.last.end,0)

println(output.sortBy(_._1))
    //List((2,10), (3,15), (7,12), (12,0), (15,10), (20,8), (24,0))

  }


}
