package my.coding.practise


/**
  * Created by subhankardeysarkar on 4/15/18.
  */
object MinimumSetOverlapInterval extends App {

  //val intervals = Array(Interval(2, 5), Interval(3, 6) , Interval(3, 7) , Interval(4, 6), Interval(6, 9), Interval(6, 7), Interval(7, 8), Interval(7, 11), Interval(7, 10), Interval(11, 12))

  val input = Array(Array(1,2), Array(2,3),Array(2,4),Array(4,5))

  var arrIntervals : Array[Interval] = _

  var minSetLengthAtEnd = Integer.MAX_VALUE
  var outSet : Set[Int] = _

  val xx = intersectionSizeTwo(input)

  println(xx)
 println(outSet)


  def intersectionSizeTwo(intervals: Array[Array[Int]]): Int = {

    arrIntervals = intervals.map(x=> Interval(x(0),x(1)))
    getMinSet(0,Set.empty[Int])
    minSetLengthAtEnd
  }


  def getTwoCombinations(intvl:Interval) : List[Set[Int]] = {

    var lst : List[Set[Int]] = Nil

    for(i<- intvl.start to  intvl.end)
      for (j<- i + 1 to  intvl.end)
        {
          if(i!=j)
            lst = lst ::: List(Set(i,j))
        }

    lst

  }

  def return2MinEachSet(set1:Set[Int], set2: Set[Int] ) : Set[Int] = {

    set1 union  set2

  }


  def getMinSet(indx:Int, s:Set[Int] ) : Unit = {

    if(indx == arrIntervals.length )
      {

        if(s.size < minSetLengthAtEnd && s.size >= 2)
          {
            minSetLengthAtEnd = s.size
            outSet = s
          }

      }
    else {

      val sets = getTwoCombinations(arrIntervals(indx))
      sets.foreach( x => getMinSet(indx + 1, s.union(x) ) )

    }

  }




}



//case class Interval(x:Int,y:Int)