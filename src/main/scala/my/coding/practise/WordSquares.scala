package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/30/17.
  */
object WordSquares {


  val hm = new collection.mutable.HashMap[String, Array[String]]()

  def getkeyVal(arr: Array[String], indx: Int): (String, String) = {
    var j = 0
    var retStr1 = ""
    var retStr2 = ""
    while (j <= arr.length - 1) {
      if (j <= indx)
        retStr1 += arr(j)
      else
        retStr2 += arr(j)
      j += 1
    }
    (retStr1, retStr2)
  }

  def copyToSquareArray(arr: Array[Array[String]], str: String, topIndex: Int):Array[Array[String]] = {

    var inpArr = str.split("")
    for (i <- inpArr.indices) {
      arr(i)(topIndex) = inpArr(i)
    }
    arr(topIndex) = inpArr
    arr
  }

  def makeSquare(arr:Array[Array[String]],indx:Int,lstVisted:List[String]):Unit = {

    var retList:List[String] = Nil

    if(indx == arr.length)
      {
        //last call with final word
        lstVisted.map(println)
        println("")
      }
    else {

      var j = 0
      var searchWord = ""
      while (j < indx) {
        searchWord += arr(j)(indx)
        j += 1
      }
    //  println(s"searchWord=$searchWord")

      val getAllPossibleWordsAtThisIndex = hm.get(searchWord)

      if (getAllPossibleWordsAtThisIndex.isDefined) {
        getAllPossibleWordsAtThisIndex.get.foreach { f =>
          val word = searchWord + f
        //  if (!lstVisted.contains(word)) {
            val narr = copyToSquareArray(arr, word, indx )
            makeSquare(narr, indx + 1, lstVisted ::: List(word))
     //     }
        }
      }

    }



  }

  def main(args: Array[String]): Unit = {

   val input = Array("ball", "area", "lead", "lady")
 //    val input = Array("abat","baba","atan","atal")
  //  val input = Array("area","lead","wall","lady","ball")

    val wordLength = input.head.length
    var squareArray = Array.ofDim[String](wordLength, wordLength)

    // make a hashtable with trie kind of structure
    input.foreach { x =>
      val arr = x.split("")
      var j = 0
      while (j <= arr.length - 2) {
        val ret = getkeyVal(arr, j)
        hm.put(ret._1, hm.getOrElse(ret._1, Array.empty[String]) ++ Array(ret._2))
        j+=1
      }

    }

   // hm.iterator.foreach(u => println(u._1 + " -> " + u._2.mkString(",")))


        var k=0
        while(k<input.length) // loop every word and try every one as the first word
          {

            squareArray = copyToSquareArray(squareArray,input(k),0)
            makeSquare(squareArray,1,List(input(k)))
            k +=1
          }


  }

}
