package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/2/17.
  * https://leetcode.com/problems/number-of-digit-one/description/
  * number 1 appears in 1, 10-19, 21,31....91, 100,101,111-119,121..
  * 0 - 9 -> 1
  * 10 - 99 -> 18
  * 100 - 999 -> 18*9
  * 1000 - 9999 -> 18*9*9
  * incomplete
  */


object NumberOfDigitOne {

  def getNumberOfOnes(n:Int):Int = {

    val len = n.toString.length
    var sum =0
    for(i <- 0 until  len - 1)
      {
        sum += getNumberOfBuckets(i)
      }

    val remainingVals = n%10^(len -1)
    sum += getNumberOfOnes(remainingVals)

sum

  }

  def getNumberOfBuckets(n:Int):Int = {

  if(n == 0)
    return 1
    else
  {
    if(n == 1)
      return 18
    else
      return 9*getNumberOfBuckets(n-1)
  }

  }




  def main(args: Array[String]): Unit = {

    println(getNumberOfOnes(1000))



  }

}
