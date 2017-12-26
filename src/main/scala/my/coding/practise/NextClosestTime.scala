package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/24/17.
  * https://leetcode.com/problems/next-closest-time/description/
*/


object NextClosestTime  {

  val input = "23:59"
  val pattern = "(\\d)(\\d)\\:(\\d)(\\d)".r
  val pattern(hh1,hh2,mm1,mm2) = input

  val h1 = hh1.toInt
  val h2 = hh2.toInt
  val m1 = mm1.toInt
  val m2 = mm2.toInt

  val inpArray = Array(h1,h2,m1,m2).sorted
  val minNumber = input.head


  def getNextValid(num:Int, higherBound:Int) : (Boolean, Int) = {

    if(num == higherBound)
      {
        (false , -1)
      }
    else {
     val arr1 = inpArray.filter(c => c>num && c<=higherBound)
      if(arr1.length > 0)
        {
          (true,arr1.head)
        }
      else
        {
          (false,-1)
        }
    }

  }


  def main(args: Array[String]): Unit = {

    // check if there exists a next valid minute
    val min2Check = getNextValid(m2,9)
    if(min2Check._1)
      {
        println(s"Next Closest Time ${h1}${h2}:${m1}${min2Check._2}")
        return
      }

    val m1check = getNextValid(m1,5)
    if(m1check._1)
      {
        println(s"Next Closest Time ${h1}${h2}:${m1check._2}${minNumber}")
        return
      }

    val h2check = if(h1 == 0 || h1 == 1) getNextValid(h2,9) else getNextValid(h2,3)
    if(h2check._1)
      {
        println(s"Next Closest Time ${h1}${h2check._2}:${minNumber}${minNumber}")
        return
      }

    val h1check = getNextValid(h1,2)
    if(h1check._1)
    {
      println(s"Next Closest Time ${h1check._2}${minNumber}:${minNumber}${minNumber}")
      return
    }

    println(s"Next Closest Time ${minNumber}${minNumber}:${minNumber}${minNumber}")


  }





}
