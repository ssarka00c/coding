package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/25/17.
  */
object Dijkstras_Algorithm {

  case class edge(v1:String,v2:String,distance:Int)
  val graph = List(
    edge("a","b",4),
    edge("a","c",3),
    edge("a","e",7),
    edge("c","b",6),
    edge("c","d",11),
    edge("c","e",8),
    edge("b","d",5),
    edge("e","d",2),
    edge("e","g",5),
    edge("d","f",2),
    edge("d","g",10),
    edge("g","f",3)
  )

  val startVrtx = "a"
  var visitedList:List[(String,Int)] = Nil
  var vertices = graph.flatMap(x=> List(x.v1,x.v2)).distinct.map((_,Int.MaxValue))


  def getMinVertex():Option[( String, Int )]  = {        // get the min from vertices and remove it
    if(vertices.isEmpty)
    None
    else {
      val vrtx = vertices.minBy(_._2)
      vertices = vertices.filter(x => !x._1.equalsIgnoreCase(vrtx._1))
      Some(vrtx)
    }
  }

  def updateVertexDistance(v:String,distance:Int) = {     // update the vertices list with the new distance only if it has lower distnce than current

    val vrtx = vertices.filter(y => y._1.equalsIgnoreCase(v))
    if(vrtx.length ==1)
      {
        if(vrtx.head._2 > distance)
          vertices = vertices.filter(y => !y._1.equalsIgnoreCase(v)) ::: List((vrtx.head._1,distance))
      }
  }

  def addToVistedSet(v:String, distance:Int)  = {
    visitedList ++= List((v,distance))
  }

  def getOtherVertex(v:String,e:edge):String = {
    if(e.v2 == v)
      e.v1
    else
      e.v2
  }



  def main(args: Array[String]): Unit = {

    // add start vertex to visted set with distance zero and remove it from vertices
    visitedList ++= List((startVrtx,0))
    vertices = vertices.filter(x=> !x._1.equalsIgnoreCase(startVrtx))
    graph.filter( g=> g.v2.equalsIgnoreCase(startVrtx) || g.v1.equalsIgnoreCase(startVrtx))
      .map( x=> (getOtherVertex(startVrtx,x),x) ).foreach(z=> updateVertexDistance(z._1,z._2.distance))

    var curVertex = getMinVertex()
    while(curVertex.isDefined)
      {
          addToVistedSet(curVertex.get._1,curVertex.get._2)
          val connectedVertices = graph.filter(g => g.v2.equalsIgnoreCase(curVertex.get._1) || g.v1.equalsIgnoreCase(curVertex.get._1))
            .map( x=> (getOtherVertex(curVertex.get._1,x),x) )
          val unVistedVertices =  connectedVertices.filter(c=> !visitedList.map(_._1).contains(c._1)) // filter vertices that are present in visited list
          unVistedVertices.foreach(z=> updateVertexDistance(z._1, z._2.distance + curVertex.get._2))

        curVertex = getMinVertex()
      }

    println(s"Minimum Distance From ${startVrtx} to all other Vertices")
    println("-"*60)
    println(visitedList)

  }


}
