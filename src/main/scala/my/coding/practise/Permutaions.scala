package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/24/17.
  */
object Permutaions {


  def dropAt(index:Int,arr:Array[Int]) : Array[Int] = {

   arr.zipWithIndex.filter(_._2 != index).map(_._1)

  }

  def generatePermutations(lst:List[Array[Int]]) : List[Array[Int]] = {

    var retList:List[Array[Int]] = Nil

    for(arr <- lst)
      {
        if(arr.length ==2)
          {
            retList = retList ++ List( Array(arr(0),arr(1)), Array(arr(1),arr(0)))
          }
        else {

          for (i <- arr.indices) {
            val elem = arr(i)
            val restArray = dropAt(i, arr)
            retList = retList ++ generatePermutations(List(restArray)).map(x => Array(elem) ++ x)
          }

        }
      }

retList

  }

  def main(args: Array[String]): Unit = {

   val output =  generatePermutations(List(Array(1,2,3)))
    output.map(x=>println(x.mkString(",")))
    println(s"Number Of Combinations = ${output.length}")
  }


}
