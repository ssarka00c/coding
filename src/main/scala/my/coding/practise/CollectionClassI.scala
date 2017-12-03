package my.coding.practise

import scala.collection.mutable

/**
  * Created by subhankardeysarkar on 12/3/17.
  * https://leetcode.com/problems/insert-delete-getrandom-o1-duplicates-allowed/description/
  * Idea is to create a HashMap with key as the integer item and Value as the array of iteger items
  * anytime you add an item just add to the array
  * anytime you remove, if the array becomes empty after removal of the value, remove the key as well
  * for random , just choose a random key, you can also choose a randome value from for the key but that is not necessary since the key itself is the random number
  */

class myCollection
{
  var arr = new collection.mutable.ArrayBuffer[Int]()

  def add(n:Int):myCollection = {
    arr += n
    this
  }

  def remove(n:Int):myCollection = {

    var found = false
    var keepLooping = true
    var i = 0
    while(keepLooping && !found)
    {
      if(arr(i) == n)
        {
          found=true
          keepLooping = false
          //drop the element from the array , this will create a hole , swap the hole with the last element and all set
          arr.remove(i)
        }

      i +=1
      if(i >= arr.length -1)
        keepLooping = false
    }
    this
  }

  def getRandom:Int = {
    val r = scala.util.Random
    if(arr.length > 1) {
      val rIndx = r.nextInt(arr.length - 1)
      arr(rIndx)
    }
    else
      {
        arr(0)
      }
  }

  def getObject = this


  override def toString: String = arr.mkString(",")

}

object CollectionClassI {

  val hm = new mutable.HashMap[Int,myCollection]()
  val r = scala.util.Random

  def addItem(n:Int) = {

    //check if the item already exists
    // if it des not add it to hashmap
    // if it does , extract myCollection, add to stack class and add the myCollection back
    println("Added Item " + n )
    if (hm.get(n).isDefined)
      hm.put(n, hm(n).add(n))
    else {
      val myc = new myCollection
      hm.put(n, myc.add(n))
    }
    println(hm)


  }

  def removeItem(n:Int) = {
      if (hm.get(n).isDefined) {
        val myc = hm(n).remove(n)
        if (myc.arr.isEmpty)
          hm.remove(n)
        else
          hm.put(n, myc)
      }
    println("Removed Item = " + n)
    printHM()
  }

    def getRandomItem:Int = {
      val rInt = r.nextInt(hm.keys.size)
      val arr = hm.keys.toArray
      val ret = hm(arr(rInt)).getRandom
      println("Random Item = " + ret)
      ret
    }

def printHM() = {
 println(hm)
}



  def main(args: Array[String]): Unit = {


    addItem(2)
    addItem(3)
    addItem(4)
    addItem(3)
    addItem(3)
    addItem(2)
    addItem(5)
    addItem(6)
    removeItem(2)
    removeItem(3)
    removeItem(2)
    removeItem(2)
    getRandomItem
    getRandomItem

    /*
    Results
    Added Item 2
Map(2 -> 2)
Added Item 3
Map(2 -> 2, 3 -> 3)
Added Item 4
Map(2 -> 2, 4 -> 4, 3 -> 3)
Added Item 3
Map(2 -> 2, 4 -> 4, 3 -> 3,3)
Added Item 3
Map(2 -> 2, 4 -> 4, 3 -> 3,3,3)
Added Item 2
Map(2 -> 2,2, 4 -> 4, 3 -> 3,3,3)
Added Item 5
Map(2 -> 2,2, 5 -> 5, 4 -> 4, 3 -> 3,3,3)
Added Item 6
Map(2 -> 2,2, 5 -> 5, 4 -> 4, 3 -> 3,3,3, 6 -> 6)
Removed Item = 2
Map(2 -> 2, 5 -> 5, 4 -> 4, 3 -> 3,3,3, 6 -> 6)
Removed Item = 3
Map(2 -> 2, 5 -> 5, 4 -> 4, 3 -> 3,3, 6 -> 6)
Removed Item = 2
Map(5 -> 5, 4 -> 4, 3 -> 3,3, 6 -> 6)
Removed Item = 2
Map(5 -> 5, 4 -> 4, 3 -> 3,3, 6 -> 6)
Random Item = 5
Random Item = 6

     */


  }


}
