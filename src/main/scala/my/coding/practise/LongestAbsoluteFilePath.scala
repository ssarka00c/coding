package my.coding.practise

import scala.collection.mutable

/**
  * Created by subhankardeysarkar on 12/27/17.
  */
object LongestAbsoluteFilePath {

  case class FileInfo(level:Int,rPath:String)

  var resultFilePath:(String,Int) = ("",0)
  val stack = new collection.mutable.Stack[FileInfo]()
  //var fullPathArray:Array[String] = Array.empty[String]
  var currentLevelFileMap = new mutable.HashMap[Int,String]()

  def main(args: Array[String]): Unit = {

    val input = """"dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext"""
    //val input = """dir\n\tsubdir1\n\tsubdir2\n\t\tfile.ext"""
 //   val input = "file.ext"
    val allPaths = input.split("\\\\n",-1).map { x =>
      val y = getNumberOfBackslashTab(x)
      FileInfo(y._1,y._2)
    }

    for(p <- allPaths) {
      pushFileInfo(p)
//      println(p)
//      println(currentLevelFileMap)
//      println(stack.toArray.mkString("->"))
//      println("-"*100)
    }

    println(resultFilePath)



  }

  def getNumberOfBackslashTab(str:String):(Int,String) = {
    val replacedStr= str.replaceAll("\\\\t","")
    (  (str.length - replacedStr.length) / 2  , replacedStr )
  }

  def checkAndAddressFile = {
    if(stack.nonEmpty)
      {
        if(stack.top.rPath.contains("."))
          {
              val curPath = currentLevelFileMap.iterator.toArray.filter(x => x._1 <= stack.top.level).sortBy(_._1).map(_._2).mkString("/")
              if (curPath.length > resultFilePath._2)
                resultFilePath = (curPath, curPath.length)
          }
      }
  }


  def pushFileInfo(fileInfo:FileInfo) : Unit= {

    var keepLooping = true

    if( stack.isEmpty)
      {
        stack.push(fileInfo)
        currentLevelFileMap.put(fileInfo.level,fileInfo.rPath)
        checkAndAddressFile
        return
      }

      if(fileInfo.level > stack.top.level)
        {
          stack.push(fileInfo)
          currentLevelFileMap.put(fileInfo.level,fileInfo.rPath)
          checkAndAddressFile
        }
    else {

      while (keepLooping) {
        stack.pop()

        if (stack.isEmpty) {
          keepLooping = false
        }
        else {
          if (stack.top.level < fileInfo.level) {
            stack.push(fileInfo)
            currentLevelFileMap.put(fileInfo.level,fileInfo.rPath)
            checkAndAddressFile
            keepLooping = false
          }

        }
      }


    }


  }




}
