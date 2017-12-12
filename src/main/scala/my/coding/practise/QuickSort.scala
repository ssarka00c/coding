package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/11/17.
  */
object QuickSort {

  var inputArrar = Array(23,76,445,90,123,645,998,6765,9,33,78,45)

  def quickSort(startIndex:Int,endIndex:Int):Unit = {

    var i = startIndex - 1
    var j = startIndex
    val partIndex = endIndex
    if(startIndex == endIndex)
      return
    else
      {
        if(endIndex - startIndex == 1)
          {
            if(inputArrar(startIndex) > inputArrar(endIndex))
              swap(startIndex,endIndex)
            return
          }
        else
          {
            for(j<- startIndex until endIndex)
              {
                if(inputArrar(j) < inputArrar(partIndex))
                  {
                    //increment i and swap, i and j
                    i +=1
                    swap(i,j)
                  }
              }

            val partitionPoint = i + 1
            swap(partIndex,partitionPoint)
            //insert the partition at position i and work on two different partitions
            //bring the partitionIndex data to position i+1
            if(partitionPoint > startIndex)
              {
                // has a partition on the left
                quickSort(startIndex,partitionPoint - 1)
               // println(s"quikSort($startIndex,${partitionPoint-1} = ${inputArrar.mkString(",")})")
              }
            if(partitionPoint < endIndex)
              {
                // has a right partition
                quickSort(partitionPoint+1,endIndex)
               // println(s"quikSort(${partitionPoint+1},$endIndex) = ${inputArrar.mkString(",")}")
              }



          }
      }


  }

  def swap(x:Int,y:Int) = {

    if(x!=y) {
      inputArrar(x) = inputArrar(y) + inputArrar(x)
      inputArrar(y) = inputArrar(x) - inputArrar(y)
      inputArrar(x) = inputArrar(x) - inputArrar(y)
     // println(inputArrar.mkString(","))
    }

  }

  def main(args: Array[String]): Unit = {

    quickSort(0,inputArrar.length-1)
    println("Final Sorted Output = " + inputArrar.mkString(","))

  }

}
