package my.coding.practise

/**
  * Created by subhankardeysarkar on 1/29/18.
  * Given a list of input tasks to run, and the cooldown interval, output the minimum number of time slots required to run them.
// Tasks: 1, 1, 2, 1, 2
// Recovery interval (cooldown): 2
// Output: 8 (order is 1 _ _ 1 2 _ 1 2 )
Whats the time and space complexity ? What's the ideal case of space complexity ?


  */
object TaskScheduler1 {


  class Scheduler(coolPeriod:Int)
  {

    var set =  new collection.mutable.TreeSet[Int]()

    def addToScheduler(task:Int) : Boolean =
    {
      if(set.contains(task))
        false
      else
        {
          set.add(task)
          true
        }
    }

    def executeTasks(): Unit =
    {
      if(set.isEmpty)
        return


      var str = ""
      set.foreach{ i =>
        str += s" ${i}"
      }

      if(set.size <= coolPeriod)
        {
          for(i <- 1  to coolPeriod + 1 - set.size )
          str +=" _"
        }

      set = set.empty
      print(str)

    }

  }

  def main(args: Array[String]): Unit = {

    val sch = new Scheduler(2)
    for( i<- Array(1,1,2,1,2))
      {
        if(!sch.addToScheduler(i)) {
          sch.executeTasks()
          sch.addToScheduler(i)
        }
      }
    sch.executeTasks()


  }


}
