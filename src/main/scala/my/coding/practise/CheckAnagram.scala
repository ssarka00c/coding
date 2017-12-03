package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/3/17.
  * https://leetcode.com/problems/scramble-string/description/
  * give every char of a string value 1, and group by and sum so it forms a->1, b->1, c->1... etc
  * compare two strings by filtering one map from the other which should result in empty set and the string sizes be same
  */
object CheckAnagram {


  def checkAnagram(str1:String,str2:String) : Boolean = {

    val sarr1 = str1.split("").map((_,1))
    val sarr2 = str2.split("").map((_,1))

    val charCount1 = sarr1.groupBy(_._1).mapValues(_.map(_._2).sum)
    val charCount2 = sarr2.groupBy(_._1).mapValues(_.map(_._2).sum)

    //check if key values in both list macth and if you filter one from other, it becomes empty

    val filterSet = charCount1.filter{ k=>
      if( charCount2.get(k._1).isDefined )
        !(charCount2(k._1) == k._2)
      else
        true

    }

    filterSet.isEmpty && charCount1.size == charCount2.size

  }


  def main(args: Array[String]): Unit = {

    println( checkAnagram("abcdx","dbca") )  // returns -> false
    println( checkAnagram("subha","uhasb") )  // returns -> true
    println( checkAnagram("mom","dad") )      // returns -> false

  }

}
