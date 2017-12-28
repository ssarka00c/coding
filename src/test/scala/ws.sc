
def getNumberOfBackslashTab(str:String):Int = {
  (str.length - str.replaceAll("\\\\t","").length) / 2
}

val str = """abc\n\tabc""".split("\\\\n",-1)
.map(x=> (getNumberOfBackslashTab(x),x))
