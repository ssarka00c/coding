package my.coding.practise

/**
  * Created by subhankardeysarkar on 1/1/18.
  */
object SubArraySum {

  def main(args: Array[String]): Unit = {

    val inputArray = Array(-2,6,6,-4,-5,11,-30,-25)
    var tempArray = Array.emptyIntArray
    var subArrays:List[(Array[Int],Int,Int)] = Nil
    var si =0
    var ei =0
    var nsi =0

   var maxSoFar = 0
    var max_here = 0

    for(i <- inputArray.indices)
      {
        max_here += inputArray(i)

        if(max_here < 0)
          {
            max_here = 0
            si = i + 1
          }
        if(max_here > maxSoFar)
          {
            maxSoFar = max_here
            ei = i
            nsi = si
          }
      }

    println("Start Index=" + nsi + ", End Index = " + ei + ", Max Sum=" + maxSoFar)

  }

}
