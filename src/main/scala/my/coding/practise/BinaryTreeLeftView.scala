package my.coding.practise

import scala.collection.mutable


/**
  * Created by subhankardeysarkar on 12/3/17.
  * Considering -1 indicates empty places in the tree
  * Not really a binary treee just the view building mechanism
  */



object BinaryTreeLeftView {


  val hm = new mutable.HashMap[Int,Int]() // Level , Value
  val tree = Array(0,6,4,16,1,5,8,24,-1,99,-1,-1,-1,-1,-1,-1,-1,-1,-1,999)
  val rootNodePos = 1

  def iterateTreeDepthFirst(level:Int,pos:Int):Unit = {

    if(pos < tree.length && tree(pos) != -1) {


      if (hm.get(level).isEmpty) {
        hm.put(level, tree(pos))
      }
      iterateTreeDepthFirst(level + 1, 2 * pos) // Left Child First
      iterateTreeDepthFirst(level + 1, (2 * pos) + 1)

    }

  }


  def main(args: Array[String]): Unit = {

    iterateTreeDepthFirst(0,rootNodePos)
    println(hm.keys.toArray.sorted.map(x => hm.getOrElseUpdate(x,-1)).mkString(","))


  }


}
