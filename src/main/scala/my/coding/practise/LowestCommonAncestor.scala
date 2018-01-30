package my.coding.practise

import scala.util.control.Breaks._

/**
  * Created by subhankardeysarkar on 1/29/18.
  */


case class Node(value:Int)
{
  var leftChild:Option[Node] = None
  var rightChild:Option[Node] = None
}

object LowestCommonAncestor {

  var path1:List[Node] = Nil

  val n10=Node(10)
  val  n5 = Node(5)
  val n7 = Node(7)
  val n2 = Node(2)
  val n4 = Node(4)
  val n6 = Node(6)
  val n8 = Node(8)
  val n11 = Node(11)
  val n15 = Node(15)
  val n18 = Node(18)
  val n20 = Node(20)

  val root = n10
  n10.leftChild = Some(n5)
  n10.rightChild = Some(n15)
  n5.leftChild = Some(n2)
  n5.rightChild = Some(n6)
  n2.rightChild = Some(n4)
  n15.leftChild = Some(n11)
  n15.rightChild = Some(n20)
  n11.rightChild = Some(n18)

  // finally have a tree now

  def getNode(n:Node, path:List[Node]):Unit=
  {

    val curNode = path.reverse.head

    if(n.value == curNode.value)
      {
        path1 = path
      //  println(path1)
      }
    else {

      if (curNode.leftChild.isDefined) {
        getNode(n, path ::: List(curNode.leftChild.get))
      }
      if (curNode.rightChild.isDefined) {
        getNode(n, path ::: List(curNode.rightChild.get))
      }

    }

  }

  def getOtherChild(n:Node, child:Node): Option[Node] =
  {
    var ret:Option[Node] = None
    if(n.leftChild.isDefined)
      {
        if(n.leftChild.get == child && n.rightChild.isDefined)
          ret = n.rightChild
      }
    if(n.rightChild.isDefined)
      {
        if(n.rightChild.get == child && n.leftChild.isDefined)
          ret = n.leftChild
      }

    ret
  }


  def checkIfTreeContains(root:Node, n:Node) : Boolean = {

    var v1 = false
    var v2 = false

    if(root.value == n.value)
      true
    else {
      if (root.leftChild.isDefined)
        v1 = checkIfTreeContains(root.leftChild.get, n)
      if (root.rightChild.isDefined)
        v2 = checkIfTreeContains(root.rightChild.get, n)

      v1 || v2
    }

  }




  def main(args: Array[String]): Unit = {

    val input1 = n4
    val input2 = n18

     getNode(input1,List(root))

    if(path1.isEmpty)
      {
        println(s"Node $n20 Not Found")
      }
    else {
      var isFound = false
      breakable {
        for (x <- path1.reverse) {
          if (checkIfTreeContains(x, input2)) {
            println(s"Lowest Common Ancestoe is $x")
            isFound = true
            break()
          }
        }
      }
      if(!isFound)
        {
          println(s"Node $input2 Not Found")
        }
    }



  }






}
