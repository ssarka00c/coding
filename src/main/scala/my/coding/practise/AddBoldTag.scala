package my.coding.practise

/**
  * Created by subhankardeysarkar on 1/2/18.
  * https://leetcode.com/problems/add-bold-tag-in-string/description/
  *
  */
object AddBoldTag {

  var lstIntervals:List[(Int,Int)] = Nil

  def returnMatch(arr:Array[String],searchStr:String):List[(Int,Int)] = {

    val inpLength = searchStr.length
    var i = 0
    var keepLooping = true
    var retLst:List[(Int,Int)] = Nil
    val indexedArray = arr.zipWithIndex
    while (keepLooping)
      {

        if( indexedArray.filter(x=> x._2 >= i && x._2 <= (i+inpLength -1)).map(_._1).mkString("") == searchStr)
          {
            retLst ++= List((i,i+inpLength-1))
          }

        if(i == (arr.length - inpLength))
          keepLooping = false

        i+=1
      }

    retLst
  }

  def pushInterval(interval:(Int,Int)) = {

    var tList:List[(Int,Int)] = Nil
    var tList2:List[(Int,Int)] = Nil
    var min1overlap = false

    if(lstIntervals.nonEmpty) {
      lstIntervals.foreach { x =>
        //check if there is overlap
        val hasOverlap = x match {
          case nooverlap if interval._1 > x._2 + 1 || interval._2 + 1 < x._1 => false
          case _ => true
        }

        if (hasOverlap) {
          tList ++= List(x) ::: List(interval)
          min1overlap = true
        }
        else
          tList2 ++= List(x)
      }

      if(!min1overlap)
        tList2 ++= List(interval)

      if (tList.nonEmpty)
        lstIntervals = tList2 ::: List((tList.map(_._1).min, tList.map(_._2).max))
      else
        lstIntervals = tList2

    }
    else
      lstIntervals = List(interval)

  }





  def main(args: Array[String]): Unit = {

    val dict = Array("aaa","aab","bc")
    val str = "aaabbcc"
//    val dict = Array("ab","123")
//    val str = "abcxyz123"
    var inputStrArray = str.split("")
    val zippedArray = inputStrArray.zipWithIndex
    var getAllMatches:List[(Int,Int)] = Nil

    for(itm <- dict)
      {
        getAllMatches ++= returnMatch(inputStrArray,itm)
      }

    // Now we have all the intervals that matches the dictionary
    // sort by start index and fold to merge

    val allIntervals = getAllMatches.sortBy(_._1).toArray
    for( x <- allIntervals )
      {
        pushInterval(x)
      }

    //lstIntervals now have all non overlapping intervals which needs to be bolded
    var finalString = str
    var start=0
    for(i <- lstIntervals)
      {
        inputStrArray(i._1) = "<b>" + inputStrArray(i._1)
        inputStrArray(i._2) = inputStrArray(i._2) +  "</b>"
      }

    println(s"Bolded String = ${inputStrArray.mkString("")}")
    /*
    Bolded String = <b>abc</b>xyz<b>123</b>
    Bolded String = <b>aaabbc</b>c
     */

  }

}
