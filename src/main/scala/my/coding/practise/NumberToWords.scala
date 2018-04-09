object NumberToWords extends App {

  val inputNumber = 123456798

  println(millions(inputNumber))

  def ones(n:Int):String = if(n>0) getWord(n) else ""
  def tens(n:Int):String = {
    val str = n.toString
    if(str.length>=2)
      {
        if(n>=10 && n<=20)
          getWord(n)
        else {
          val x = str.slice(str.length - 2, str.length).toInt
          val f1 = x % 10
          val t = x - f1
          (if (t > 0) getWord(t) else "") + " " + ones(f1)
        }
      }
    else
      ones(n)
  }
  def hundreds(n:Int):String = {

    val str = n.toString
    if(str.length>=3)
      {
        val x = str.slice(str.length - 3, str.length).toInt
        val f1:Int = x/100
        (if(f1 > 0) getWord(f1) + "Hundred " else "")  + tens(x - 100*f1)
      }
    else
      tens(n)

  }
  def thousands(n:Int) : String = {
    if(n.toString.length>=4)
      {
        val str = n.toString
        val l = str.length
        var sl = 6
        if(l<6)
          sl = l
        val x = str.slice(l-sl,l-3).toInt

        ( if(x>0) hundreds(x) + "Thousand " else "" ) + hundreds(n)

      }
    else
      hundreds(n)
  }
  def millions(n:Int) : String = {
    if(n.toString.length>=7)
      {
        val str = n.toString
        val l = str.length
        var sl = 9
        if(l<9)
          sl = l
        val x = str.slice(l-sl,l-6).toInt

        ( if(x>0) hundreds(x) + "Million " else "")  + thousands(n)
      }
    else
      thousands(n)
  }


  def getWord(n:Int) : String = {
    n match
      {
      case 1000000 => "Million "
      case 100000 => "Hundred Thounsand "
      case 1000 => "Thousand "
      case 100 => "Hundred "
      case 90 => "Ninety "
      case 80 => "Eighty "
      case 70 => "Seventy "
      case 60 => "Sixty "
      case 50 => "Fifty "
      case 40 => "Fourty "
      case 30 => "Thirty "
      case 20 => "Twenty "
      case 19 => "Nineteen "
      case 18 => "Eighteen "
      case 17 => "Seventeen "
      case 16 => "Sixteen "
      case 15 => "Fifeteen "
      case 14 => "Fourteen "
      case 13 => "Thirteen "
      case 12 => "Twelve "
      case 11 => "Eleven "
      case 10 => "Ten "
      case 9 => "Nine "
      case 8 => "Eight "
      case 7 => "Seven "
      case 6 => "Six "
      case 5 => "Five "
      case 4 => "Four "
      case 3 => "Three "
      case 2 => "Two "
      case 1 => "One "
      case 0 => "Zero "
      case _ => "Unknown "

    }
  }




}
