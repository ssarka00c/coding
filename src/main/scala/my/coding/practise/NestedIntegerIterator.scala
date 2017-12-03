package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/2/17.
  *  Uber Question Nested Integer Iteretor
  */

case class NestedInteger(n:Int)
{
  def getInteger:Int = n
  var lst:List[NestedInteger] = _
}

class NestedIntegerIterator(input:List[NestedInteger]) {

  val stack = new collection.mutable.Stack[NestedInteger]()
  input.reverse.foreach(stack.push)

  def hasNext :Boolean = {
    if(stack.isEmpty) false else true
  }

  def next:Int = {
    if(hasNext)
      {
        val nextItem = stack.pop()
        if(nextItem.lst != null)
          {
            nextItem.lst.reverse.foreach(stack.push)
          }
        nextItem.n
      }
    else
      {
        -1 //throw Exception
      }
  }

}

object NestedIntegerProgram
{



  def main(args: Array[String]): Unit = {

    val ni1 = NestedInteger(1)
    val ni2 = NestedInteger(2)
    val ni3 = NestedInteger(3)
    val ni4 = NestedInteger(4)
    val ni5 = NestedInteger(5)
    val ni6 = NestedInteger(6)
    ni1.lst = List(ni2,ni3)
    ni2.lst = List(ni4)
    val input = List(ni1,ni5,ni6)

  val iter = new NestedIntegerIterator(input)

    while(iter.hasNext)
      println(iter.next)

  }
}


