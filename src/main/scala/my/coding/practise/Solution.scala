package my.coding.practise

import scala.collection.mutable

/**
  * Created by subhankardeysarkar on 4/13/18.
  */
// main method in "Solution" will be run as your answer
object Solution {

  // the root directory
  val root = Directory("root")
  //var defaultDir = root
  var dirNavigator = new mutable.Stack[Directory]()
  dirNavigator.push(root)

  def main(args: Array[String]) {
    //Enter your code here. Read input from STDIN. Print output to STDOUT

    //Reading Number of Inputs
    var totalTests = 0
    var arrUserInputs : Array[String] = Array.empty
    try
    {
      totalTests = scala.io.StdIn.readLine().trim.toInt
    }
    catch {
      case e:Exception =>
        println("Invalid Command")
        return
    }

    var i =1
    while(i <= totalTests) {
      val userCmd = scala.io.StdIn.readLine()


      if (userCmd.trim.length > 0) {
        val cmd = getCommandAndArgument(userCmd.trim)
        cmd._1 match {

          case "mkdir" =>
            if (cmd._2.isDefined) {
              val ret = mkdir(cmd._2.get)
              println(ret.msg)
            }
            else {
              println("Invalid Command")
            }


          case "cd" =>
            if (cmd._2.isDefined) {
              val ret = cd(cmd._2.get)
              println(ret.msg)
            }
            else {
              println("Invalid Command")
            }

          case "up" =>
            val ret = up
            println(ret.msg)

          case "dir" =>
            val ret = dir
            println(ret.msg)

          case _ => println("Invalid Command")
        }


        i += 1
      }

    }
  }


  def getCommandAndArgument(line:String) : (String,Option[String]) = {

    val trimmedLine = line.trim
    var retCommand = ""
    var retArgument = ""


     val filteredLine = trimmedLine.split(" ",-1).filter(_.length>0)

    if(filteredLine.isEmpty || filteredLine.length > 2)
     return ("Invalid Command",None)



    if(filteredLine.length == 2)
      {
        if(filteredLine(1).length > 6)
          ("Invalid Command",None)
        else
          (filteredLine(0),Some(filteredLine(1)))
      }
    else
      (filteredLine(0),None)



  }

  def mkdir(dir:String):ReturnMsg = {

    if(dir.length == 0 || dir.length > 6)
      return ReturnMsg(s"Command: mkdir    ${rightPad(dir,9)}\nInvalid Command",false) // invalid Directory name

    val defaultDir = dirNavigator.head

    //check if a sub directory already exist by the name
    if(defaultDir.subDirs.nonEmpty)
      {
        if(defaultDir.subDirs.count(x => x.dirName == dir) > 0)
          {
            return ReturnMsg("Command: mkdir    " + rightPad(dir,9) + "\nSubdirectory already exists",false)
          }
      }

    defaultDir.subDirs = defaultDir.subDirs ::: List(Directory(dir))
    ReturnMsg("Command: mkdir    " + rightPad(dir,9),true)

  }

  def cd(dir:String):ReturnMsg = {

    if(dir.length == 0 || dir.length > 6)
      return ReturnMsg(s"Command: cd       ${rightPad(dir,9)}\nInvalid Command",false) // invalid Directory name

    var defaultDir = dirNavigator.head

    if(defaultDir.subDirs.isEmpty) {
      if(defaultDir == root)
        return ReturnMsg(s"Command: cd       ${rightPad(dir,9)}\nNo subdirectories", false)
      else
        ReturnMsg(s"Command: cd       ${rightPad(dir,9)}\nSubdirectory does not exist",false)
    }

      //return ReturnMsg("Subdirectory does not exist")

    val destDir = defaultDir.subDirs.filter(x => x.dirName == dir )
    if(destDir.isEmpty)
      {
         ReturnMsg(s"Command: cd       ${rightPad(dir,9)}\nSubdirectory does not exist",false)
      }
    else
      {
        defaultDir = destDir.head
        dirNavigator.push(defaultDir)
        ReturnMsg(s"Command: cd       ${rightPad(dir,9)}",true)
      }

  }

  def up:ReturnMsg = {

    val defaultDir = dirNavigator.head

    if(defaultDir == root)
      {
        return ReturnMsg(s"Command: ${rightPad("up",9)}\nCannot move up from root directory",false)
      }

    if(dirNavigator.isEmpty)
      return ReturnMsg(s"Command: ${rightPad("up",9)}\nCannot move up from root directory",false)

    dirNavigator.pop()
    ReturnMsg(s"Command: ${rightPad("up",9)}",true)

  }

  def dir : ReturnMsg = {

    val defaultDir = dirNavigator.head

    if(defaultDir.subDirs.isEmpty)
      {
        val ret = "Command: dir      \nDirectory of " + dirNavigator.toArray.map(_.dirName).mkString("\\") + ":\n"
        return ReturnMsg(ret + "No subdirectories",false)
      }

    var i =1
    var retStr = ""

    // format printable string as desired
    defaultDir.subDirs.sortBy(_.dirName).foreach{ z=>
      retStr += rightPad(z.dirName,9)
      i +=1
      if(i==10)
        {
          i=1
          retStr += "\n"
        }

    }

    //command output absolute directory path
    retStr = "Command: dir      \nDirectory of  " + dirNavigator.toArray.reverse.map(_.dirName).mkString("\\") + ":\n" + retStr

    ReturnMsg(retStr,true)


  }

  def rightPad(str:String,n:Int):String = str.padTo(n," ").mkString

  case class Directory(dirName:String)
  {
    var subDirs:List[Directory] = Nil
  }

  case class ReturnMsg(msg:String,retStatus:Boolean)

}