package yml2stats

/** Captions of the tables: the title and notes. */
object Captions {
  /** @param notes sentences after the title */
  def apply(title : String, notes : String*) : String =
    (s"$title." +: notes).mkString(" ")

  /** Prints a caption below a text table, in lines of 100 characters. */
  def printText(caption : String) : Unit = {
    println()
    val lines = caption.split(" ").foldLeft(Vector("")) { (lines, word) =>
      if (lines.last.isEmpty) lines.init :+ word
      else if (lines.last.length + 1 + word.length <= 100)
        lines.init :+ (lines.last + " " + word)
      else lines :+ word
    }
    lines.foreach(println)
  }
}
