package my.coding.practise

import scala.collection.mutable

/**
  * Created by subhankardeysarkar on 12/24/17.
  * https://leetcode.com/problems/k-empty-slots/description/
  * There is a garden with N slots. In each slot, there is a flower. The N flowers will bloom one by one in N days. In each day, there will be exactly one flower blooming and it will be in the status of blooming since then.
 **
 Given an array flowers consists of number from 1 to N. Each number in the array represents the place where the flower will open in that day.
 **
 For example, flowers[i] = x means that the unique flower that blooms at day i will be at position x, where i and x will be in the range from 1 to N.
 **
 Also given an integer k, you need to output in which day there exists two flowers in the status of blooming, and also the number of flowers between them is k and these flowers are not blooming.
 **
 If there isn't such day, output -1.
 **
 Example 1:
    *Input:
    *flowers: [1,3,2]
    *k: 1
    *Output: 2
    *Explanation: In the second day, the first and the third flower have become blooming.
    *Example 2:
    *Input:
    *flowers: [1,2,3]
    *k: 1
    *Output: -1
 *
  *
  * Sollution
  * Sorted Linked List
  * 1 -> 2 -> 3
  * 1->3
  * when inserting an element , find its right spot and check to see if the gap with neighbours meet the criteria
  *
 */
object KEmptySlots {

  val root = node(-1,0)
  val k =1
  var validDays = Array.emptyIntArray


  case class node(element:Int,day:Int)
  {
    var nextNode : Option[node] = None
    var previousNode : Option[node] = None
  }

  def insertNode(curr:node, item:node):Boolean = {      // this Int will propagate the day from the Check Call

    try {
      val previousNode = curr.previousNode.get
      previousNode.nextNode = Some(item)
      item.previousNode = Some(previousNode)
      item.nextNode = Some(curr)
      curr.previousNode = Some(item)
      checkWithNeighboursForKConditionTrue(item)
      true
    }
    catch{
      case _ => false
    }

  }



  def insertIntoLinkedList(element:Int,day:Int) = {

    if(root.nextNode.isEmpty)
      {
        val n = node(element,day)
        root.nextNode = Some(n)
        n.previousNode = Some(root)
        checkWithNeighboursForKConditionTrue(n)
      }
    else
      {
        // iterate till you find the element bigger than current element
        var currentNode = root
        var keepLooping = true
        while(keepLooping)
        {
          val nextNode = currentNode.nextNode
          if(nextNode.isDefined)
            {
              val nextElement = nextNode.get.element
              if(element < nextElement) {
                insertNode(nextNode.get, node(element, day))
                keepLooping = false
              }
              else
                {
                  currentNode = nextNode.get
                }
            }
          else
            {
              // reached the end node just add to end
             val n = node(element,day)
              currentNode.nextNode = Some(n)
              n.previousNode = Some(currentNode)
              checkWithNeighboursForKConditionTrue(n)
              keepLooping = false
            }

        }

      }


  }

  def checkWithNeighboursForKConditionTrue(n:node) =     // returns the day when this happens
  {
    if(n.previousNode.isDefined)
      if( n.element - n.previousNode.get.element == k + 1 && n.previousNode.get.element != -1)
                 validDays = validDays ++ Array(n.day)


    if(n.nextNode.isDefined)
      if(n.nextNode.get.element - n.element == k + 1) {
        validDays = validDays ++ Array(n.day)
      }


  }

def printLinkedList(r:Option[node]) = {

  if(r.isDefined) {
    var keepLooping = true
    var arr: Array[Int] = Array.emptyIntArray
    var currentNode = r
    while (currentNode.get.nextNode.isDefined) {
      arr = arr ++ Array(currentNode.get.nextNode.get.element)
      currentNode = currentNode.get.nextNode
    }
println("Root -> " + arr.mkString(" -> "))
  }
}


  def main(args: Array[String]): Unit = {

    val input = List((1,1),(3,2),(2,3))
  //  val input2 = List( (5,1), (6,2),(3,3),(19,4),(7,5),(8,6))
    var dayCounter = 1
    input.foreach { x =>
      insertIntoLinkedList(x._1, x._2)
      println("Day " + dayCounter  )
      dayCounter +=1
      printLinkedList(Some(root))
    }

    println("Valid Days = " + validDays.mkString(","))

  }

}


/*
Output 1

Day 1
Root -> 1
Day 2
Root -> 1 -> 3
Day 3
Root -> 1 -> 2 -> 3
Valid Days = 2

Output 2
Day 1
Root -> 5
Day 2
Root -> 5 -> 6
Day 3
Root -> 3 -> 5 -> 6
Day 4
Root -> 3 -> 5 -> 6 -> 19
Day 5
Root -> 3 -> 5 -> 6 -> 7 -> 19
Day 6
Root -> 3 -> 5 -> 6 -> 7 -> 8 -> 19
Valid Days = 3
 */