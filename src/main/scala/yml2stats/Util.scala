package yml2stats

object Util {
  def sanitizeString(s: String): String = {
    s.replaceAll("_", "\\\\_")
  }

  /* Read a file that contains a list of directories per line,
     e.g.,
     a/b/name1.c
     c/d/name2.c

     And then create a map from base file names to their root directories, i.e.,
     Map(("name1" -> "a/b"),
         ("name2" -> "c/d"))
     Base file names are the name of the files without its extension.
   */

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
