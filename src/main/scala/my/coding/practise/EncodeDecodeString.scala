package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/31/17.
  */
object EncodeDecodeString {

  val serializerSeperator = ','

  def encode(lst:List[String]):String = {

    lst.map{ x=>
     x.replaceAll(",","\\\\,")
    }.mkString(",")
  }


  def decode(str:String):List[String] = {

    var retList:List[String] = Nil
    var pc = 'x'
    var curStr = ""
    for(c<-str)
      {
        if(pc != '\\' && c == serializerSeperator)
          {
            retList ++= List(curStr.replaceAll("\\\\,",","))
            curStr=""
          }
        else
        {
          curStr +=c
        }

          pc = c
      }

    if(curStr.length>0)
      retList ++= List(curStr)

retList

  }

  def main(args: Array[String]): Unit = {

    val input = List("a\",uhasihd89yr8hdqknbc","xyzdasdas?????--348092i0√ \"\\\"\"4","009")
    println(s"Input = ${input}")
    //println(decode(encode(input)))
    val encodedStr = encode(input)
    println(s"Encoded String = ${encodedStr}")
    val decodedList = decode(encodedStr)
    println(s"Decoded List = ${decodedList}")

  }



}
