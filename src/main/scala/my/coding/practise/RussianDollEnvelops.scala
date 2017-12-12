package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/11/17.
  */

case class Envelop(lenght:Int, width:Int)
{
  var totalEnvelops = 1
  var lst:List[Envelop] = List(this)
  def pushSmallerEnvelop(envlop:Envelop): Unit =
  {
    if(envlop.lenght < lenght && envlop.width < width) {
      totalEnvelops += 1
      lst = lst ::: List(envlop)
    }
  }


}

object RussianDollEnvelops extends App{

  val envelops = List(Envelop(5,4),Envelop(6,4),Envelop(6,7),Envelop(2,3))

  envelops.foreach{ e=>

    envelops.foreach( x=> x.pushSmallerEnvelop(e))

  }

  val envlp =  envelops.map(x=> (x,x.totalEnvelops)).maxBy(_._2)
  println("Maximum Envelops = " +envlp._2)
  println("The Envelops = " + envlp._1.lst)





}
