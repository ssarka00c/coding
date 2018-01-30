package my.coding.practise

/**
  * Created by subhankardeysarkar on 1/28/18.
  */
object BinaryNumber {


  def main(args: Array[String]): Unit = {

    import Math._
    val input = 255d
    val base = 2
    var p = 8
    var keepLooping = true
    var remainingNumber = input
    var arr:Array[Int] = Array.ofDim[Int](8)

    if(remainingNumber > pow(2,8))
      {
        println("Number Cannot Be Represented in 8 bit")
        System.exit(1)
      }

    while(keepLooping)
      {

        remainingNumber = remainingNumber - pow(base,p)
        if(remainingNumber > 0)
          {
            arr(8-p) = 1
            p-=1
          }
        else
          {
            if(remainingNumber == 0)
              {
                val idx = 8-p
                arr(idx) = 1
                keepLooping = false
                p -=1
              }
            else
              {
                arr(8-p) = 0
                remainingNumber = remainingNumber + pow(base,p)
                p-=1
              }
          }

//        if(p<=0)
//          keepLooping = false

      }

    println(arr.mkString(","))



  }


}
