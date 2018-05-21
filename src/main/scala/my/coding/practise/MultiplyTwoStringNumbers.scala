package my.coding.practise

/**
  * Created by subhankardeysarkar on 5/20/18.
  */
object MultiplyTwoStringNumbers extends App{

  val input1="60"
  val input2 = "60"

  val r1 = input1.split("").map(_.toInt)
  val r2 = input2.split("").map(_.toInt)

  val n = r2.length
//  var ri = r1.length -1
  var out = Array.ofDim[Int](r2.length,r1.length+r2.length+1)
  var dummy = Array.ofDim[Int](r1.length+r2.length+1)
  var oi = 0

  for(i <- n -1 to 0 by -1 )
    {
      var outArr = Array.ofDim[Int](oi)
      var c = 0

      for( j<- r1.length -1 to 0 by -1)
        {
          val p = r2(i) * r1(j)
          val fp = p + c

          if(j==0)
            {
              outArr = Array(fp/10, fp%10) ++ outArr
              out(oi) = rpad0(outArr)
              oi +=1
              c=0
            }
          else
            {
              outArr = Array(fp%10) ++ outArr
              c = fp/10
            }

        }


    }

  out.foreach(x => dummy = summArray(dummy,x))

  println(s"Output = ${dummy.mkString("").toLong}")




  def rpad0(arr:Array[Int]):Array[Int]= {

    Array.ofDim[Int](out(0).length - arr.length) ++ arr

  }


  def summArray(a1:Array[Int],a2:Array[Int]) : Array[Int] = {

    var c =0
    var outArr = Array.empty[Int]

    var x1 = a1
    var x2 = a2

    if(a1.length > a2.length)
      x2 = Array.ofDim[Int](a1.length - a2.length) ++ a2
    else{
      if(a2.length > a1.length)
        x1 = Array.ofDim[Int](a2.length - a1.length) ++ a1
    }

    for(i<- x1.length -1 to 0 by -1)
      {
        val s = x1(i) + x2(i) + c
        if(i ==0)
          {
            outArr = Array(s/10,s%10) ++ outArr
          }
        else
        {
          c = s/10
          outArr = Array(s%10) ++ outArr
        }

      }

    outArr


  }


}
