package yml2stats

import yml2stats.Benchmarks._
import yml2stats.Main.{Col, filterCols, sanitizeToolNameForLatex}
import yml2stats.Settings._

// tables over the groups of several inputs, solved meaning solved with a
// correct verdict
object GroupTables {
  // sat/unsat per group and tool, None for a tool without runs in the group
  private type Cells = Seq[Seq[Option[(Int, Int)]]]

  private def solved(runs : RunInfos) : (Int, Int) =
    (runs.correctSatRuns.length, runs.correctUnsatRuns.length)

  private def runsOf(group : Group, tool : Summary) : Option[RunInfos] =
    group.toolRuns.find(_._1.fullToolName == tool.fullToolName).map(_._2)

  /**
   * Prints the solved benchmarks per group and tool.
   * @param noBest the tools left out of the best values of a group, which are
   *               bold in LaTeX
   */
  def printOverview(groups : Seq[Group], tools : Seq[Summary],
                    noBest : Set[String], latex : Boolean) : Unit = {
    val cells = groups.map(group =>
      tools.map(tool => runsOf(group, tool).map(solved)))
    if (latex)
      printLatex(groups, tools, cells, Some(noBest),
                 "Solved Benchmarks per Group (Sat/Unsat)", "tbl:solved-per-group")
    else
      printText(groups, tools, cells)
  }

  /**
   * The benchmarks of the group solved by the tool and by no other tool,
   * None without runs in the group; a portfolio is compared with the tools
   * that are not its members, a tool not with portfolios.
   * @param portfolioMembers the names of the members of each portfolio, by
   *                         the name of the portfolio
   */
  def uniquelySolved(group : Group, tool : Summary,
                     portfolioMembers : Map[String, Set[String]]) : Option[RunInfos] =
    for (runs <- runsOf(group, tool)) yield {
      val members = portfolioMembers.getOrElse(tool.fullToolName, Set())
      val others = group.toolRuns.filter { case (other, _) =>
        other.fullToolName != tool.fullToolName &&
        !members(other.fullToolName) &&
        !portfolioMembers.contains(other.fullToolName)
      }
      others.foldLeft(runs)(_ - _._2)
    }

  def printUnique(groups : Seq[Group], tools : Seq[Summary],
                  portfolioMembers : Map[String, Set[String]],
                  latex : Boolean) : Unit = {
    val cells = groups.map(group => tools.map(tool =>
      uniquelySolved(group, tool, portfolioMembers).map(solved)))
    if (latex)
      printLatex(groups, tools, cells, None,
                 "Uniquely Solved Benchmarks per Group (Sat/Unsat)",
                 "tbl:uniquely-solved")
    else
      printText(groups, tools, cells)
  }

  // per pair of tools a row with the solved benchmarks and the memouts of
  // each, and the benchmarks solved by only one of them
  def printComparison(pairs : Seq[((Summary, RunInfos), (Summary, RunInfos))],
                      latex : Boolean) : Unit = {
    def toolCols(key : String, header : String,
                 tool : Summary, runs : RunInfos) : Seq[Col] = {
      val (sat, unsat) = solved(runs)
      Seq(Col(key, header,
              if (latex) sanitizeToolNameForLatex(tool.toolName)
              else tool.fullToolName),
          Col("sat", "Sat", s"$sat"),
          Col("unsat", "Unsat", s"$unsat"),
          Col("memout", "Memout", s"${runs.memoutRuns.length}"))
    }
    def satUnsat(runs : RunInfos) = {
      val (sat, unsat) = solved(runs)
      s"$sat/$unsat"
    }
    val data = pairs.map { case ((tool, runs), (other, otherRuns)) =>
      filterCols(toolCols("tool", "Tool", tool, runs) ++
                 toolCols("other", "Other", other, otherRuns) ++
                 Seq(Col("tool-only", "Tool only", satUnsat(runs - otherRuns)),
                     Col("other-only", "Other only", satUnsat(otherRuns - runs))))
    }
    if (data.isEmpty) return
    val header = data.head.map(_.header)
    val rows = data.map(_.map(_.value))
    val colSpec = data.head.map(col =>
      if (col.key == "tool" || col.key == "other") "l" else "r").mkString
    if (latex)
      printLatexTable(header, rows, colSpec,
                      "Comparison of Two Tools (Solved Only: Sat/Unsat)",
                      "tbl:tool-comparison")
    else
      printTextTable(header, rows)
  }

  // the benchmarks solved by a tool and a base tool with one verdict: their
  // number, those solved faster and slower by the tool, and the median and
  // the geometric mean of the time ratio tool/base, None without ratios
  case class RuntimeComparison(both : Int, faster : Int, slower : Int,
                               median : Option[Double],
                               geometricMean : Option[Double])

  /**
   * Compares the durations of the runs of a tool with those of a base tool.
   * @param solvedRuns the runs solved with the verdict, e.g. `_.correctSatRuns`
   */
  def runtimeComparison(runs : RunInfos, baseRuns : RunInfos,
                        solvedRuns : RunInfos => Seq[RunInfo]) : RuntimeComparison = {
    val baseDurations =
      solvedRuns(baseRuns).map(run => run.bmBaseName -> run.duration).toMap
    val durations = for (run <- solvedRuns(runs);
                         baseDuration <- baseDurations.get(run.bmBaseName))
                    yield (run.duration, baseDuration)
    // a duration of 0 s, as in logs in whole seconds, has no ratio
    val ratios = durations.collect {
      case (duration, base) if duration > 0 && base > 0 => duration / base
    }.sorted
    val n = ratios.length
    RuntimeComparison(
      durations.length,
      durations.count { case (duration, base) => duration < base },
      durations.count { case (duration, base) => duration > base },
      if (n == 0) None
      else Some((ratios((n - 1) / 2) + ratios(n / 2)) / 2),
      if (n == 0) None
      else Some(math.exp(ratios.map(math.log).sum / n)))
  }

  /**
   * Prints per tool and verdict the comparison of its durations with those of
   * the base tool over all groups.
   * @param tools the rows of the tools compared with the base tool
   */
  def printRuntimes(base : (Summary, RunInfos), tools : Seq[(Summary, RunInfos)],
                    latex : Boolean) : Unit = {
    def name(tool : Summary) =
      if (latex) sanitizeToolNameForLatex(tool.toolName) else tool.fullToolName
    def ratio(value : Option[Double]) =
      value.map("%.2f".formatLocal(java.util.Locale.ROOT, _)).getOrElse("-")
    val verdicts = Seq[(String, RunInfos => Seq[RunInfo])](
      ("sat", _.correctSatRuns), ("unsat", _.correctUnsatRuns))
    val data = for ((tool, runs) <- tools; (verdict, solvedRuns) <- verdicts)
    yield {
      val comparison = runtimeComparison(runs, base._2, solvedRuns)
      filterCols(Seq(
        Col("tool", "Tool", name(tool)),
        Col("verdict", "Verdict", verdict),
        Col("both", "Both", s"${comparison.both}"),
        Col("faster", "Faster", s"${comparison.faster}"),
        Col("slower", "Slower", s"${comparison.slower}"),
        Col("median", "Median", ratio(comparison.median)),
        Col("geomean", "Geo. mean", ratio(comparison.geometricMean))))
    }
    if (data.isEmpty) return
    val header = data.head.map(_.header)
    val rows = data.map(_.map(_.value))
    val colSpec = data.head.map(col =>
      if (col.key == "tool" || col.key == "verdict") "l" else "r").mkString
    if (latex)
      printLatexTable(header, rows, colSpec,
                      s"Runtimes Compared with ${name(base._1)} " +
                        s"(Time Ratio Tool/${name(base._1)})",
                      "tbl:runtimes")
    else
      printTextTable(header, rows)
  }

  // one header row, as in the other LaTeX tables
  private def printLatexTable(header : Seq[String], rows : Seq[Seq[String]],
                              colSpec : String,
                              caption : String, label : String) : Unit = {
    println("% For this table, please include \\usepackage{booktabs} in your LaTeX preamble.")
    val latexTableString =
      s"""\\begin{table}[h]
         |  \\centering
         |  \\caption{$caption}
         |  \\label{$label}
         |  \\begin{tabular}{$colSpec}
         |    \\toprule
         |    ${header.mkString(" & ")} \\\\ \\midrule
         |    ${rows.map(_.mkString(" & ") + " \\\\").mkString("\n    ")}
         |    \\bottomrule
         |  \\end{tabular}
         |\\end{table}
         |""".stripMargin
    println(latexTableString)
  }

  private def totals(cells : Cells) : Seq[Option[(Int, Int)]] =
    cells.transpose.map(column =>
      Some((column.flatten.map(_._1).sum, column.flatten.map(_._2).sum)))

  // the label and the number of benchmarks of each group and of the total
  private def rowLabels(groups : Seq[Group]) : Seq[(String, Int)] =
    groups.map(group => (group.name, group.benchmarkNames.size)) :+
      (("Total", groups.map(_.benchmarkNames.size).sum))

  private def labelCols(label : String, benchmarks : Int) : Seq[Col] =
    filterCols(Seq(Col("group", "Group", label),
                   Col("benchmarks", "Benchmarks", s"$benchmarks")))

  private def printText(groups : Seq[Group], tools : Seq[Summary],
                        cells : Cells) : Unit = {
    val data = rowLabels(groups).zip(cells :+ totals(cells)).map {
      case ((label, benchmarks), values) =>
        labelCols(label, benchmarks) ++ tools.zip(values).map {
          case (tool, value) =>
            Col("tool", tool.fullToolName,
                value.map { case (sat, unsat) => s"$sat/$unsat" }.getOrElse("-"))
        }
    }
    printTextTable(data.head.map(_.header), data.map(_.map(_.value)))
  }

  private[yml2stats] def printTextTable(header : Seq[String],
                                        rows : Seq[Seq[String]]) : Unit = {
    val colWidths = (header +: rows).transpose.map(col => col.map(_.length).max)

    def pad(s: String, width: Int) = s.padTo(width, ' ')

    println(header.zip(colWidths).map { case (h, w) => pad(h, w) }.mkString(" | "))
    println(colWidths.map(w => "-" * w).mkString("-|-"))
    rows.foreach { row =>
      println(row.zip(colWidths).map { case (cell, w) => pad(cell, w) }.mkString(" | "))
    }
  }

  /**
   * Prints a group table in LaTeX, sat and unsat in two columns per tool.
   * @param noBest without it, no values in bold
   */
  private def printLatex(groups : Seq[Group], tools : Seq[Summary], cells : Cells,
                         noBest : Option[Set[String]],
                         caption : String, label : String) : Unit = {
    println("% For this table, please include \\usepackage{booktabs} in your LaTeX preamble.")
    val labelHeader = labelCols("", 0)

    def best(values : Seq[Option[(Int, Int)]]) : Option[(Int, Int)] =
      for (excluded <- noBest;
           compared = tools.zip(values).collect {
             case (tool, Some(value)) if !excluded(tool.fullToolName) => value };
           if compared.nonEmpty)
      yield (compared.map(_._1).max, compared.map(_._2).max)

    def number(value : Int, bestValue : Option[Int], tool : Summary) : String =
      if (bestValue.contains(value) && !noBest.exists(_(tool.fullToolName)))
        s"\\textbf{$value}"
      else s"$value"

    def row(label : String, benchmarks : Int, values : Seq[Option[(Int, Int)]],
            bestValues : Option[(Int, Int)]) : String =
      (labelCols(Util.sanitizeString(label), benchmarks).map(_.value) ++
         tools.zip(values).map {
           case (_, None) => "\\multicolumn{2}{c}{--}"
           case (tool, Some((sat, unsat))) =>
             number(sat, bestValues.map(_._1), tool) + " & " +
             number(unsat, bestValues.map(_._2), tool)
         }).mkString(" & ") + " \\\\"

    val groupRows = groups.zip(cells).zipWithIndex.map {
      case ((group, values), i) =>
        row(group.name, group.benchmarkNames.size, values, best(values)) +
          (if (midruleAfterGroups(group.name) && i < groups.length - 1)
             "\n    \\midrule" else "")
    }
    val totalRow =
      row("Total", groups.map(_.benchmarkNames.size).sum, totals(cells), None)

    val headerRow = (labelHeader.map(_.header) ++ tools.map(tool =>
      s"\\multicolumn{2}{c}{${sanitizeToolNameForLatex(tool.toolName)}}"))
      .mkString(" & ") + " \\\\"
    val labelColumns = labelHeader.indices.map(i => (i + 1, i + 1))
    val toolColumns = tools.indices.map(i =>
      (labelHeader.length + 2 * i + 1, labelHeader.length + 2 * i + 2))
    val rules = (labelColumns ++ toolColumns).map { case (from, to) =>
      s"\\cmidrule(lr){$from-$to}" }.mkString(" ")
    val colSpec = labelHeader.map(col => if (col.key == "group") "l" else "r")
                    .mkString + " r@{\\,/\\,}r" * tools.length

    val latexTableString =
      s"""\\begin{table}[h]
         |  \\centering
         |  \\caption{$caption}
         |  \\label{$label}
         |  \\begin{tabular}{$colSpec}
         |    \\toprule
         |    $headerRow
         |    $rules
         |    ${groupRows.mkString("\n    ")}
         |    \\midrule
         |    $totalRow
         |    \\bottomrule
         |  \\end{tabular}
         |\\end{table}
         |""".stripMargin
    println(latexTableString)
  }
}
