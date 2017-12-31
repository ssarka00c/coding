var arr = Array.ofDim[String](10,10)

arr(0)(0) = "a"
arr(0)(1) = "b"
arr(1)(0) = "c"
arr.map(_.mkString(",")).map(println)