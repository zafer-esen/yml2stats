package yml2stats

import yml2stats.Benchmarks._
import yml2stats.Main.{Col, ColGroup, filterCols, sanitizeToolNameForLatex}
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
    // a note on bold values only with tools left out of them
    val excluded = tools.filter(tool => noBest(tool.fullToolName))
    val bold =
      if (!latex || excluded.isEmpty) Nil
      else Seq("The largest numbers of each group, without " +
               excluded.map(tool => sanitizeToolNameForLatex(tool.toolName))
                       .mkString(", ") + ", are in bold.")
    val caption = Captions("Benchmarks solved as sat / unsat per group", bold : _*)
    if (latex)
      printLatex(groups, tools, cells, Some(noBest), caption, "tbl:solved-per-group")
    else
      printText(groups, tools, cells, caption)
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
    val caption = Captions(
      "Benchmarks solved as sat / unsat by one tool and by no other, per group",
      "A portfolio is compared with the tools that are not its members.")
    if (latex)
      printLatex(groups, tools, cells, None, caption, "tbl:uniquely-solved")
    else
      printText(groups, tools, cells, caption)
  }

  // per pair of tools a row with the solved benchmarks and the memouts of
  // each, and the benchmarks solved by only one of them
  def printComparison(pairs : Seq[((Summary, RunInfos), (Summary, RunInfos))],
                      latex : Boolean) : Unit = {
    def toolCols(key : String, header : String, group : ColGroup,
                 tool : Summary, runs : RunInfos) : Seq[Col] = {
      val (sat, unsat) = solved(runs)
      Seq(Col(key, header, toolCell(tool, latex)),
          Col("sat", "Sat", s"$sat", Some(group)),
          Col("unsat", "Unsat", s"$unsat", Some(group)),
          Col("memout", "Memout", s"${runs.memoutRuns.length}", Some(group)))
    }
    def satUnsat(runs : RunInfos) = {
      val (sat, unsat) = solved(runs)
      s"$sat/$unsat"
    }
    val only = Some(ColGroup("only-group", "Solved only"))
    val data = pairs.map { case ((tool, runs), (other, otherRuns)) =>
      filterCols(
        toolCols("tool", "Tool", ColGroup("tool-group", "Tool"), tool, runs) ++
        toolCols("other", "Other", ColGroup("other-group", "Other"),
                 other, otherRuns) ++
        Seq(Col("tool-only", "Tool only", satUnsat(runs - otherRuns), only),
            Col("other-only", "Other only", satUnsat(otherRuns - runs), only)))
    }
    if (data.isEmpty) return
    val rows = data.map(_.map(_.value))
    val colSpec = data.head.map(col =>
      if (col.key == "tool" || col.key == "other") "l" else "r").mkString
    val onlyGroup = data.head.find(_.key == "tool-only").flatMap(_.group)
    val caption = Captions("Comparison of pairs of tools",
      onlyGroup.map(group => s"The numbers under ${group.header} are sat / unsat.").toSeq : _*)
    if (latex)
      printLatexTable(data.head, Seq(rows), colSpec, caption, "tbl:tool-comparison")
    else
      printTextTable(data.head.map(_.header), rows, caption)
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
   * Prints the runtime comparisons over all groups in one table, a row per
   * base, tool and verdict.
   * @param comparisons the rows of the base tools, each with the rows of the
   *                    tools compared with it
   */
  def printRuntimes(comparisons : Seq[((Summary, RunInfos), Seq[(Summary, RunInfos)])],
                    latex : Boolean) : Unit = {
    val counts = Some(ColGroup("benchmarks-group", "Benchmarks"))
    val ratios = Some(ColGroup("ratio-group", "Time ratio"))
    val blocks = for ((base, tools) <- comparisons)
    yield for ((tool, runs) <- tools; (verdict, solvedRuns) <- RunInfos.verdicts)
    yield {
      val comparison = runtimeComparison(runs, base._2, solvedRuns)
      filterCols(Seq(
        Col("tool", "Tool", toolCell(tool, latex)),
        Col("base", "Base", toolCell(base._1, latex)),
        Col("verdict", "Verdict", verdict),
        Col("both", "Both", s"${comparison.both}", counts),
        Col("faster", "Faster", s"${comparison.faster}", counts),
        Col("slower", "Slower", s"${comparison.slower}", counts),
        Col("median", "Median", number(comparison.median, "%.2f"), ratios),
        Col("geomean", "Geo. mean", number(comparison.geometricMean, "%.2f"), ratios)))
    }
    val data = blocks.filter(_.nonEmpty)
    if (data.isEmpty) return
    val cols = data.head.head
    val rows = data.map(_.map(_.map(_.value)))
    val colSpec = cols.map(col =>
      if (Set("tool", "base", "verdict")(col.key)) "l" else "r").mkString
    val caption = Captions("Runtimes compared with base tools",
      "The time ratio is the duration of the tool divided by that of the base.")
    if (latex)
      printLatexTable(cols, rows, colSpec, caption, "tbl:runtimes")
    else
      printTextTable(cols.map(_.header), rows.flatten, caption)
  }

  /** The value at the share `p` of the sorted values, by the nearest rank. */
  private[yml2stats] def percentile(sorted : Seq[Double], p : Double) : Double =
    sorted(math.max(math.ceil(p * sorted.length).toInt - 1, 0))

  /**
   * The durations in seconds of the solved benchmarks of a tool, None
   * without any.
   */
  case class WallTimes(solved : Int, median : Option[Double],
                       p90 : Option[Double], p99 : Option[Double],
                       max : Option[Double], late : Int)

  /** @param lateAfter the duration after which a solved benchmark is late */
  private[yml2stats] def wallTimes(runs : RunInfos,
                                   lateAfter : Double) : WallTimes = {
    val sorted = runs.correctRuns.map(_.duration).sorted
    def at(p : Double) = if (sorted.isEmpty) None else Some(percentile(sorted, p))
    WallTimes(sorted.length, at(0.5), at(0.9), at(0.99), at(1.0),
              sorted.count(_ > lateAfter))
  }

  /**
   * The peak memory of a tool in kB over the solved benchmarks and over all
   * runs, None without such runs; runs without a peak memory in their log
   * are left out.
   */
  case class PeakMemory(solvedMedian : Option[Double], solvedP90 : Option[Double],
                        allMedian : Option[Double], allP90 : Option[Double])

  private[yml2stats] def peakMemory(runs : RunInfos) : PeakMemory = {
    def sorted(rs : Seq[RunInfo]) = rs.flatMap(_.peakMemoryKB).map(_.toDouble).sorted
    val solved = sorted(runs.correctRuns)
    val all = sorted(runs.runs)
    def at(values : Seq[Double], p : Double) =
      if (values.isEmpty) None else Some(percentile(values, p))
    PeakMemory(at(solved, 0.5), at(solved, 0.9), at(all, 0.5), at(all, 0.9))
  }

  private def toolCell(tool : Summary, latex : Boolean) : String =
    if (latex) sanitizeToolNameForLatex(tool.toolName) else tool.fullToolName

  private def number(value : Option[Double], format : String) : String =
    value.map(format.formatLocal(java.util.Locale.ROOT, _)).getOrElse("-")

  /**
   * Prints the `WallTimes` per tool over all groups, late after 90% of the
   * wall time limit.
   */
  def printWallTimes(toolRuns : Seq[(Summary, RunInfos)], latex : Boolean) : Unit = {
    val durations = Some(ColGroup("durations-group", "Wall time (s)"))
    val rows = for ((tool, runs) <- toolRuns) yield {
      val times = wallTimes(runs, 0.9 * tool.wallTimeLimit)
      filterCols(Seq(
        Col("tool", "Tool", toolCell(tool, latex)),
        Col("solved", "Solved", s"${times.solved}"),
        Col("median", "Median", number(times.median, "%.1f"), durations),
        Col("p90", "90th perc.", number(times.p90, "%.1f"), durations),
        Col("p99", "99th perc.", number(times.p99, "%.1f"), durations),
        Col("max", "Maximum", number(times.max, "%.1f"), durations),
        Col("late", if (latex) "After 90\\% of limit" else "After 90% of limit",
            s"${times.late}")))
    }
    printToolTable(rows, latex, "Wall times of the solved benchmarks",
                   "tbl:wall-times", PercentileNote)
  }

  /** Prints the `PeakMemory` per tool over all groups, in MB. */
  def printPeakMemory(toolRuns : Seq[(Summary, RunInfos)], latex : Boolean) : Unit = {
    val solvedGroup = Some(ColGroup("solved-group", "Solved"))
    val allGroup = Some(ColGroup("all-group", "All runs"))
    def mb(kB : Option[Double]) = number(kB.map(_ / 1024), "%.0f")
    val rows = for ((tool, runs) <- toolRuns) yield {
      val memory = peakMemory(runs)
      filterCols(Seq(
        Col("tool", "Tool", toolCell(tool, latex)),
        Col("solved-median", "Median", mb(memory.solvedMedian), solvedGroup),
        Col("solved-p90", "90th perc.", mb(memory.solvedP90), solvedGroup),
        Col("all-median", "Median", mb(memory.allMedian), allGroup),
        Col("all-p90", "90th perc.", mb(memory.allP90), allGroup)))
    }
    printToolTable(rows, latex, "Peak memory (MB)", "tbl:peak-memory", PercentileNote,
                   "Runs without a peak memory in their log are left out.")
  }

  private val PercentileNote = "The percentiles are by the nearest rank."

  // in text, the column group in the header, without a row of groups
  private def printToolTable(rows : Seq[Seq[Col]], latex : Boolean,
                             title : String, label : String,
                             notes : String*) : Unit = {
    if (rows.isEmpty) return
    val values = rows.map(_.map(_.value))
    val caption = Captions(title, notes : _*)
    if (latex)
      printLatexTable(rows.head, Seq(values),
                      rows.head.map(col => if (col.key == "tool") "l" else "r")
                        .mkString, caption, label)
    else
      printTextTable(rows.head.map(col =>
                       col.group.map(_.header + " ").getOrElse("") + col.header),
                     values, caption)
  }

  /**
   * Prints a table with the header row of `cols` and the row of their groups
   * above it.
   * @param blocks rows separated by `\midrule`
   */
  private def printLatexTable(cols : Seq[Col], blocks : Seq[Seq[Seq[String]]],
                              colSpec : String,
                              caption : String, label : String) : Unit = {
    val rows = blocks.map(_.map(_.mkString(" & ") + " \\\\"))
                     .reduceLeft((above, below) => above ++ ("\\midrule" +: below))
    LatexTables.printTable(colSpec,
                           LatexTables.groupRows(cols) ++
                             ((cols.map(_.header).mkString(" & ") + " \\\\ \\midrule") +:
                                rows),
                           caption, label)
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
                        cells : Cells, caption : String) : Unit = {
    val data = rowLabels(groups).zip(cells :+ totals(cells)).map {
      case ((label, benchmarks), values) =>
        labelCols(label, benchmarks) ++ tools.zip(values).map {
          case (tool, value) =>
            Col("tool", tool.fullToolName,
                value.map { case (sat, unsat) => s"$sat/$unsat" }.getOrElse("-"))
        }
    }
    printTextTable(data.head.map(_.header), data.map(_.map(_.value)), caption)
  }

  /** Prints a text table with the caption below it. */
  private[yml2stats] def printTextTable(header : Seq[String],
                                        rows : Seq[Seq[String]],
                                        caption : String) : Unit = {
    val colWidths = (header +: rows).transpose.map(col => col.map(_.length).max)

    def pad(s: String, width: Int) = s.padTo(width, ' ')

    println(header.zip(colWidths).map { case (h, w) => pad(h, w) }.mkString(" | "))
    println(colWidths.map(w => "-" * w).mkString("-|-"))
    rows.foreach { row =>
      println(row.zip(colWidths).map { case (cell, w) => pad(cell, w) }.mkString(" | "))
    }
    Captions.printText(caption)
  }

  /**
   * Prints a group table in LaTeX, sat and unsat in two columns per tool.
   * @param noBest without it, no values in bold
   */
  private def printLatex(groups : Seq[Group], tools : Seq[Summary], cells : Cells,
                         noBest : Option[Set[String]],
                         caption : String, label : String) : Unit = {
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

    val groupRows = groups.zip(cells).zipWithIndex.flatMap {
      case ((group, values), i) =>
        row(group.name, group.benchmarkNames.size, values, best(values)) +:
          (if (midruleAfterGroups(group.name) && i < groups.length - 1)
             Seq("\\midrule") else Nil)
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

    LatexTables.printTable(
      colSpec, Seq(headerRow, rules) ++ groupRows ++ Seq("\\midrule", totalRow),
      caption, label)
  }
}
