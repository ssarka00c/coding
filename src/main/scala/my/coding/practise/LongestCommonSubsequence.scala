package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/30/17.
  */
object LongestCommonSubsequence {


  def getMax(a:Int,b:Int) = {
    if(a>b)
      a
    else
      b
  }

  def getLCS(str1:Array[String],str2:Array[String]):Int = {

    val indx1 = str1.length -1
    val indx2 = str2.length -1

    if(str1.isEmpty || str2.isEmpty)
      return 0
    else
      {
        if(str1(indx1) == str2(indx2))
          {
            1 + getLCS(str1.dropRight(1),str2.dropRight(1))
          }
        else
          {
            getMax(
              getLCS(str1,str2.dropRight(1)) ,
              getLCS(str1.dropRight(1),str2)
            )
          }
      }

  }


  def main(args: Array[String]): Unit = {

    val str1 = "yabcdbcax"
    val str2 = "xabcdcbay"

    println(s"LCS = ${getLCS(str1.split(""),str2.split(""))}")

  }

}
