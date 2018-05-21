object SplitAverage extends App {

  val arr = Array(1,2,3,4,5,6,7,8)
  var rootNode = Node(Nil,arr.sum,arr.length)
  var curNodes = List(rootNode)
  var isPresent = false

  var keepLooing = true
  var j = 0
  while (keepLooing)
  {
    val x = arr(j)

    var lst:List[Node]=Nil
    curNodes.foreach{ y =>
      lst = lst ::: multiply2(y,x)
    }

    if(curNodes.exists(_.goodSplit==true)) {
      println(curNodes.filter(_.goodSplit == true).mkString(" "))
      isPresent = true
     // keepLooing = false
    }
    curNodes = lst

    j+=1
    if(j == arr.length)
      keepLooing = false

  }


  def multiply2(n:Node,v:Int) : List[Node] = {
    val newNode = Node(n.v ::: List(v), arr.sum,arr.length)
    val ret = List(n, newNode)
    ret
  }

}



case class Node(v:List[Int], totalSum:Int, totalElements:Int)
{

  var goodSplit = false
  if(checkAverageEquals)
    goodSplit = true

  def checkAverageEquals:Boolean = {
    if(v.length > 0 && v.length < totalElements) {
      val res1 : Double = v.sum.toDouble/v.length
      val res2:Double = (totalSum.toDouble - v.sum.toDouble) / (totalElements - v.length)
      if (res1 == res2)
        true
      else
        false
    }
    else
      false
  }
}
