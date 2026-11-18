package yml2stats

object Util {
  def sanitizeString(s: String): String = {
    s.replaceAll("_", "\\\\_")
  }

  // a quantity such as "123456 kB", as written in the logs
  def kiloBytes(quantity : String) : Long =
    quantity.stripSuffix("kB").trim.toLong

  /* Read a file that contains a list of directories per line,
     e.g.,
     a/b/name1.c
     c/d/name2.c

     And then create a map from base file names to their root directories, i.e.,
     Map(("name1" -> "a/b"),
         ("name2" -> "c/d"))
     Base file names are the name of the files without its extension.
   */

  /* Read expected verdicts from a file with one benchmark per line: its name
     as in the logs and its expected verdict, separated by white space, e.g.,
     a/b/name1.smt2 unsat

     Empty lines and lines starting with # are skipped.
   */
  def readExpectedVerdicts(fileName : String) : Map[String, Benchmarks.Result] = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = try source.getLines().toList finally source.close()
    (for ((line, number) <- lines.map(_.trim).zipWithIndex
          if line.nonEmpty && !line.startsWith("#"))
     yield {
       val parts = line.split("\\s+")
       parts.head -> (parts.last match {
         case "sat"   => Benchmarks.True
         case "unsat" => Benchmarks.False
         case verdict => throw new Exception(
           s"$fileName:${number + 1}: expected verdict sat or unsat, not $verdict")
       })
     }).toMap
  }

  def readRootDirectoriesFromFile(fileName : String) : Map[String, String] = {
    val lines    = scala.io.Source.fromFile(fileName).getLines.toList
    val rootDirs = lines.map(line => {
      val parts    = line.split("/")
      val fileName = parts.last.dropRight(2)
      //val dirName = parts.dropRight(1).mkString("/")
      //val fullPath = line
      val fullPath = parts.dropRight(1).mkString("/").drop(2) ++ "/" ++ parts
        .last
      (fileName, fullPath)
    })
    rootDirs.toMap
  }
}
