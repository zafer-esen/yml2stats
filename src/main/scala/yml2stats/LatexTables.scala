package yml2stats

/** Tables with the rules of booktabs. */
object LatexTables {
  /**
   * The row of the column groups above the header, with a rule under each
   * group; adjacent columns of a group share a cell.
   */
  def groupRows(cols : Seq[Main.Col]) : Seq[String] = {
    if (cols.forall(_.group.isEmpty)) return Nil
    // (group, first column, last column), from 1 as in \cmidrule
    val spans = cols.zipWithIndex.foldLeft(List[(Option[Main.ColGroup], Int, Int)]()) {
      case ((group, from, _) :: rest, (col, i))
          if group.nonEmpty && col.group == group => (group, from, i + 1) :: rest
      case (spans, (col, i)) => (col.group, i + 1, i + 1) :: spans
    }.reverse
    val cells = spans.map {
      case (Some(group), from, to) =>
        s"\\multicolumn{${to - from + 1}}{c}{${group.header}}"
      case (None, _, _) => ""
    }
    val rules = spans.collect {
      case (Some(_), from, to) => s"\\cmidrule(lr){$from-$to}"
    }
    Seq(cells.mkString(" & ") + " \\\\", rules.mkString(" "))
  }

  /**
   * Prints a table float with the caption and the label, or with
   * -tex-tabular the tabular alone, the caption in a line `% caption: ...`
   * above it.
   * @param lines the lines between `\toprule` and `\bottomrule`
   * @param packages packages needed besides booktabs
   */
  def printTable(colSpec : String, lines : Seq[String], caption : String,
                 label : String, packages : Seq[String] = Nil) : Unit = {
    if (Settings.texTabularOnly) {
      println(s"% caption: $caption")
      println(s"""\\begin{tabular}{$colSpec}
                 |  \\toprule
                 |${lines.map("  " + _).mkString("\n")}
                 |  \\bottomrule
                 |\\end{tabular}""".stripMargin)
      return
    }
    printPackages("booktabs" +: packages)
    println(s"""\\begin{table}[h]
               |  \\centering
               |  \\caption{$caption}
               |  \\label{$label}
               |  \\begin{tabular}{$colSpec}
               |    \\toprule
               |${lines.map("    " + _).mkString("\n")}
               |    \\bottomrule
               |  \\end{tabular}
               |\\end{table}
               |""".stripMargin)
  }

  def printPackages(packages : Seq[String]) : Unit = {
    val usepackages = packages.map(p => s"\\usepackage{$p}")
    val list =
      if (usepackages.size == 1) usepackages.head
      else usepackages.init.mkString(", ") + " and " + usepackages.last
    println(s"% For this table, please include $list in your LaTeX preamble.")
  }
}
