

object PascalsTriangle {


  def getNextRow(arr: Array[Int]): Array[Int] = {


    var returnArray = Array.ofDim[Int](arr.length + 1)
    returnArray(0) = arr(0)
    for (i <- 0 until arr.length - 1) {
      returnArray(i + 1) = arr(i) + arr(i + 1)
    }
    returnArray(arr.length) = arr(arr.length - 1)

    returnArray

  }


  def getNthRow(n:Int) = {

    var arr = Array(1)
    if(n == 1) {
      println(s"${n}th Row = ${arr.mkString(",")}")
    }
    else
      {
        if(n>1)
          {
            for(i <- 1 to n)
              {
                arr = getNextRow(arr)
              }
            println(s"${n}th Row = ${arr.mkString(",")}")
          }
        else
          {
            println("Input Cannot Be negative")
          }
      }

  }


  def main(args: Array[String]): Unit = {

  getNthRow(1)
    getNthRow(2)
    getNthRow(3)
    getNthRow(4)
    getNthRow(5)


  }


}
