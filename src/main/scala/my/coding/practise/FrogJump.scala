package my.coding.practise

/**
  * Created by subhankardeysarkar on 12/11/17.
  * https://leetcode.com/problems/frog-jump/description/
  * Dynamic Programmin, find all valid Paths
  */



object FrogJump {

 // val inputArray = Array(0,1,2,3,4,8,9,11)
  val inputArray = Array(0,1,3,5,6,8,12,17)

  val inputValidStones = inputArray.map((_,0)).toMap
  var reachedLastStone = false

  def jump(startingStep:Int,numStepsTaken:Int, previousStones:Array[Int]) : Unit = {


    for( i <- List(numStepsTaken -1, numStepsTaken, numStepsTaken +1)) {
      if (i > 0) {
        if (isValidStep(startingStep + i)) {
          if (startingStep + i == inputArray.max) {
            println(previousStones.mkString(",") + "," + (startingStep + i))
            reachedLastStone = true
          }
          else {
            val nextStartingStep = startingStep + i
            jump( nextStartingStep, i, previousStones ++ Array(nextStartingStep))
          }
        }
      }
    }

  }


  def isValidStep(k:Int):Boolean = {
    if(inputValidStones.contains(k))
      true
    else
      false
  }

  def main(args: Array[String]): Unit = {
    jump(0,1,Array())

    if(reachedLastStone)
      println("There exists a valid path to last stone")
    else
      println("No Valid Path Found")


    /*

    Output
    1,3,5,8,12,17
    There exists a valid path to last stone

     */
  }


}
