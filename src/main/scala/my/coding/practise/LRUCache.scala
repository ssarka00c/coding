package my.coding.practise

/**
  * Created by subhankardeysarkar on 1/7/18.
  */

case class cache(var k:Int,var v:Int)
{
  var next:Option[cache] = None
  var prev:Option[cache] = None
}


class LRUCache(maxCacheLenght:Int) {

  val hm = new collection.mutable.HashMap[Int,cache]()

  val root = cache(-1,-1)
  val last = cache(-1,-1)
  root.next = Some(last)
  last.prev = Some(root)

  var cacheLength = 0

  def evitOldestNode() = {
    if(root.next.isDefined)
      {
 //       println(s"Evicted ${root.next}")
        deleteNode(root.next.get)
      }
  }

  def addLatestNode(c:cache) = {

    if(cacheLength == maxCacheLenght) {
      evitOldestNode()
    }

      c.next = Some(last)
      c.prev = last.prev
      last.prev.get.next = Some(c)
      last.prev = Some(c)
      hm.put(c.k,c)
      cacheLength += 1

   // println(s"Added To Latest ${c}")


  }

  def deleteNode(c:cache) = {

    hm.remove(c.k)
    c.prev.get.next = c.next
    c.next.get.prev = c.prev
    cacheLength -=1

 //   println(s"Deleted ${c}")
  }




  def put(key:Int,value:Int) = {

    println(s"Put Called $key $value")
    if(hm.contains(key))
      {
        deleteNode(hm(key))
      }

    if(cacheLength == maxCacheLenght)
      {
        evitOldestNode()
      }

    val newCache = cache(key,value)
//    hm.put(key,newCache)
    addLatestNode(newCache)

   // println(s"Put ${newCache}")

  }

  def get(key:Int): Int = {

    // if it exists in cache , return and move the elem to end of the linkedList
    println(s"Get Called $key ")

    if(hm.contains(key))
      {
        val ret = hm(key)
        deleteNode(ret)
        addLatestNode(ret)
        println(s"Get ${ret}")
        ret.v
      }
    else
      {
        println("Get -1")
        -1
      }

    // if not return -1

  }

  def printLinkedList() = {
    var cur = root
    var keepLooping = true
    var lst:List[cache] = Nil
    while(keepLooping)
      {
        if(cur.k != -1)
          lst ::= cur
        cur = cur.next.get

        if(cur.next.isEmpty)
          keepLooping = false

      }

    println(lst)

  }



}

object LRUCache
{
  def main(args: Array[String]): Unit = {

    val cache = new LRUCache(2)

    cache.put(1, 1)
    cache.printLinkedList()
    cache.put(2, 2)
    cache.printLinkedList()
    cache.get(1) // returns 1
    cache.printLinkedList()
    cache.put(3, 3) // evicts key 2
    cache.printLinkedList()
    cache.get(2) // returns -1 (not found)
    cache.printLinkedList()
    cache.put(4, 4) // evicts key 1
    cache.printLinkedList()
    cache.get(1)
    cache.printLinkedList()
    cache.get(3) // returns 3
    cache.printLinkedList()
    cache.get(4) // returns 4
    cache.printLinkedList()

    /*
Put Called 1 1
List(cache(1,1))
Put Called 2 2
List(cache(2,2), cache(1,1))
Get Called 1
Get cache(1,1)
List(cache(1,1), cache(2,2))
Put Called 3 3
List(cache(3,3), cache(1,1))
Get Called 2
Get -1
List(cache(3,3), cache(1,1))
Put Called 4 4
List(cache(4,4), cache(3,3))
Get Called 1
Get -1
List(cache(4,4), cache(3,3))
Get Called 3
Get cache(3,3)
List(cache(3,3), cache(4,4))
Get Called 4
Get cache(4,4)
List(cache(4,4), cache(3,3))

Process finished with exit code 0

     */


  }
}
