package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/4/17.
  */
object MaximumGap {


  def main(args: Array[String]): Unit = {

    val input = Array(7,3,2,6,8,5)
    val numBuckets = input.size -1

    val sortedArray = input.sorted

    var minNum = Int.MaxValue
    var minIndex = -1
    var maxNum = 0
    var maxIndex = -1


    minIndex = 0
    minNum = sortedArray(0)
    maxIndex = sortedArray.length -1
    maxNum = sortedArray(maxIndex)

    var newArr = dropIndex(sortedArray.toList,minIndex)
    newArr = dropIndex(newArr,maxIndex)


    val bucketInterval = (maxNum - minNum)/numBuckets
    val bucketedInput = newArr.map{ x=> (Math.floor( (x-minNum)/bucketInterval),x)}
         // .groupBy(_._1).mapValues( z=> ( z.map(_._2).min, z.map(_._2).max )).toArray.map(c=> (c._1,c._2._1,c._2._2)).sortBy(_._1)


    var maxGap = bucketedInput.head._2 - minNum + bucketInterval* (bucketedInput.head._1 - 1)
    for(i<- 0 until bucketedInput.length -1)
      {
        if(maxGap < ( bucketedInput(i+1)._2 - bucketedInput(i)._2 + bucketInterval*(bucketedInput(i+1)._1 - bucketedInput(i)._1) - 1 ) )
                maxGap = bucketedInput(i+1)._2 - bucketedInput(i)._2 + bucketInterval*(bucketedInput(i+1)._1 - bucketedInput(i)._1 - 1)

      }

    if(maxGap < maxNum - bucketedInput.last._2 + bucketInterval*(numBuckets - bucketedInput.last._1 ))

    println(bucketedInput.mkString(","))
    println(s"Maximum Gap ${maxGap}")


  }

  def dropIndex[T](list: List[T], idx: Int): List[T] =
    list.zipWithIndex.filter(_._2 != idx).map(_._1)

}
