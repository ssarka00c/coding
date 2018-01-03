package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/31/17.
  */
object EncodeDecodeString {

  val serializerSeperator = ','

  def encode(lst:List[String]):String = {

    lst.map{ x =>

     "\"" + x.replaceAll("\"","\\\\\"") + "\""

    }.mkString("")
  }


  def decode(str:String):List[String] = {

    var retList:List[String] = Nil
    var pc = 'x'
    var curStr = ""
    var openDoubleQoute = false
    for(c<-str)
      {

        if(pc != '\\' && c == '"') {
          openDoubleQoute = !openDoubleQoute
          if(!openDoubleQoute && curStr.length>0)
            {
              retList ++=List(curStr)
              curStr = ""
            }
        }
        else
          {
            if(openDoubleQoute)
              {
                if(c!='\\')
                  curStr += c
                pc = c
              }

          }


      }

    if(curStr.length>0)
      retList ++= List(curStr)

retList

  }

  def main(args: Array[String]): Unit = {

  //  val input = List("a\",uhasihd89yr8hdqknbc","xyzdasdas?????--348092i0√ \"\\\"\"4","009")
    val input = List("ab,c","xy\"z","mn\"")
    println("Input List")
    input.map(println)
    //println(decode(encode(input)))
    val encodedStr = encode(input)
    println(s"Encoded String = ${encodedStr}")
    val decodedList = decode(encodedStr)
    println("Decoded List")
    decodedList.map(println)

  }



}
