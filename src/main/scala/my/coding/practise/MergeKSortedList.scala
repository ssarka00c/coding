package my.coding.practise

/**
  * Created by subhankardeysarkar on 5/19/18.
  */
object MergeKSortedList extends App {

  val input = List(
    List(1,4,9) ,
    List(4,9,97),
    List(2,5,9),
    List(7,9,88)
  )

  var dummy:List[Int] = Nil

  input.foreach{l =>

    dummy = mergeTwoSortedList(dummy,l)

  }

  println(dummy)

  def mergeTwoSortedList(l1:List[Int],l2:List[Int]):List[Int] = {

    var i=0
    var j =0

    var kl = true
    var retList:List[Int] = Nil

    while (kl)
      {
        if(i>=l1.length && j < l2.length)
          {
            retList = retList ::: l2.slice(j,l2.length )
            kl=false
          }
        else
          {
            if(j>=l2.length && i<l1.length)
              {
                retList = retList ::: l1.slice(i,l1.length )
                kl=false
              }
            else
              {
                if(l1(i) < l2(j)) {
                  retList = retList ::: List(l1(i))
                  i += 1
                }
                else
                  {
                    retList = retList ::: List(l2(j))
                    j+=1
                  }
              }
          }
      }
retList
  }

}
