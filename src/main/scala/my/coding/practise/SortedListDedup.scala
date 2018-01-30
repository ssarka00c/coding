package my.coding.practise

/**
  * Created by subhankardeysarkar on 1/21/18.
  */
object SortedListDedup {


  def main(args: Array[String]): Unit = {

    val input = Array(1,1,2,2,2,3,4,8,9,9)
    var output:List[Int] = Nil

    var pItem:Option[Int]= None
    var curCount=0

    for(i <- input)
      {
        if(pItem.isEmpty)
          {
            pItem = Some(i)
            curCount +=1
            output ++= List(i)
          }
        else
          {
            if(pItem.get == i)
              {
                curCount +=1
              }
            else
              {
                output ++= List(i)
                pItem = Some(i)
                curCount = 1
              }
          }
      }

    println(output)


  }

}
