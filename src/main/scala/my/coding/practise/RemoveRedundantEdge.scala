package my.coding.practise

import scala.collection.mutable

/**
  * Created by subhankardeysarkar on 12/2/17.
  * https://leetcode.com/problems/redundant-connection-ii/description/
  */

case class vertex(n:Int)
{
  // defined this variable to make sure case class has the methods needed by HashMap for key comparison
  override def hashCode(): Int = n
  override def equals(any: scala.Any): Boolean = any.isInstanceOf[vertex] && any.asInstanceOf[vertex].n == n
}

object RemoveRedundantEdge {

  val htVertices = new mutable.HashMap[vertex,Tuple2[Int,Int]]()
  val lstUnRoot = new mutable.ListBuffer[Tuple2[Int,Int]]()
  val lstRedundant = new mutable.ListBuffer[Tuple2[Int,Int]]()
  var rootNode:vertex = _


  def returnEdge(lst:List[Tuple2[Int,Int]]) : Tuple2[Int,Int] = {

      lst.foreach(addToGraph)

    println(s"State of lstUnRoot = ${lstUnRoot.mkString(",")}")
    println(s"State of lstRedundant = ${lstRedundant.mkString(",")}")

    var retTuple:Tuple2[Int,Int] = (-99,-99)

    if(lstUnRoot.nonEmpty)
      retTuple = lstUnRoot.last
    else
      {
        if(lstRedundant.nonEmpty)
          retTuple = lstRedundant.last
        else
          retTuple = (-1,-1)
      }
    retTuple

  }

  def addToGraph(edge:Tuple2[Int,Int]) = {

    // check if the Edge is going to be a parent of the root , add it to unroot list and skip
    if( rootNode.n == edge._2 )
      {
        lstUnRoot += edge
      }
    else {
      //check if it is being a parent of a vertex that is already present in htVertices HashMap , skip and add to lstRedundant

      if (htVertices.get(vertex(edge._2)).isDefined)
        {
          lstRedundant += edge
        }
      else
        {
          // if they do not exists and do not violate any un root property add it to htVertises which represents all vertices that has a parent
          htVertices.put(vertex(edge._2), edge)
        }
    }

  }



  def main(args: Array[String]): Unit = {

    val graphInput = List((1,2),(2,3),(3,4),(4,1),(1,5),(2,4))    //List(Tuple2(1,2),Tuple2(1,3),Tuple2(2,3))

      if(graphInput.isEmpty)
        {
          println("The List is Empty, Invalid Request")
        }
    else
        {
          // first add the root node , consider the first tuple , first element is root node
          rootNode = vertex(graphInput(0)._1)
          println( s"Output = ${returnEdge(graphInput)}" )
        }

  }


}
