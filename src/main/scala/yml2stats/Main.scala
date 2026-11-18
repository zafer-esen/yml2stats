package yml2stats

import scala.io.Source
import net.jcazevedo.moultingyaml._
import yml2stats.Benchmarks._
import Benchmarks.MyYamlProtocol._
import yml2stats.parser._

import java.util.Date
import Settings._

import scala.collection.mutable.ArrayBuffer
//import org.apache.logging.log4j.core.Filter.Result

object Main {

  def printWarning(s : String) = {
    if(verbosityLevel >= 1)
      println(s)
  }
  def printInfo(s : String) = {
    if(verbosityLevel >= 2)
      println(s)
  }

  private def makeUniqueBaseNames(runs: Seq[RunInfo]): Seq[RunInfo] = {
    val grouped = runs.groupBy(_.bmBaseName)

    grouped.flatMap { case (baseName, collisions) =>
      if (collisions.size <= 1) {
        collisions
      } else {
        // map each run to its path parts (reversed)
        val runPaths = collisions.map(r => r -> r.bmName.split("/").reverse).toMap
        val depths = scala.collection.mutable.Map(collisions.map(_ -> 1): _*)

        var unique = false
        var currentNames = Seq[(RunInfo, String)]()

        while (!unique) {
          currentNames = collisions.map { r =>
            val parts = runPaths(r)
            val d = math.min(depths(r), parts.length)
            val name = parts.take(d).reverse.mkString("-")
            r -> name
          }

          val nameCounts = currentNames.map(_._2).groupBy(identity).map { case (k, v) => k -> v.size }
          val collisionsFound = nameCounts.filter(_._2 > 1).keys.toSet

          if (collisionsFound.isEmpty) {
            unique = true
          } else {
            // Try to resolve collisions by increasing depth
            var changed = false
            collisions.foreach { r =>
              val name = currentNames.find(_._1 == r).get._2
              if (collisionsFound.contains(name)) {
                val parts = runPaths(r)
                if (depths(r) < parts.length) {
                  depths(r) += 1
                  changed = true
                }
              }
            }

            if (!changed) {
              val collidingRunNames = collisions
                .filter(r => collisionsFound.contains(currentNames.find(_._1 == r).get._2))
                .map(_.bmName)
              throw new RuntimeException(s"Could not generate unique names for the following benchmarks (paths might be identical or sub-paths of each other): ${collidingRunNames.mkString(", ")}")
            }
          }
        }

        currentNames.map { case (r, name) =>
          if (name != r.bmBaseName) r.copy(uniqueBaseName = Some(name)) else r
        }
      }
    }.toSeq
  }

  private[yml2stats] def sanitizeToolNameForLatex(toolName: String): String = {
    Settings.latexToolNameReplacements.getOrElse(toolName, toolName.replace("_", "\\_"))
  }

  private def renameHeader(header: String): String = {
    Settings.headerRenames.getOrElse(header, header)
  }

  private def isColumnHidden(colKey: String): Boolean = {
    Settings.hiddenColumns.contains(colKey.toLowerCase)
  }

  private[yml2stats] case class Col(key: String, header: String, value: String)

  private[yml2stats] def filterCols(cols: Seq[Col]): Seq[Col] = {
    cols.filterNot(c => isColumnHidden(c.key)).map(c => c.copy(header = renameHeader(c.header)))
  }

  private def sanitizeEncodingForLatex(encoding: String): String = {
    val baseEncoding = getCleanEncodingName(encoding)
    Settings.latexEncodingReplacements.get(baseEncoding) match {
      case Some(replacement) => replacement
      case None => encoding.replace("_", "\\_")
    }
  }

  // the rows by name, or with -tools in the order of the selection
  private def inTableOrder(runs: Seq[(Summary, RunInfos)]): Seq[(Summary, RunInfos)] =
    if (selectedTools.isEmpty) runs.sortBy(_._1.fullToolName) else runs

  // a run killed at a cgroup memory limit has no error message, only its
  // peak memory close to the limit
  private def isMemoryKill(run : RunInfoRaw, duration : Double,
                           summary : Summary) : Boolean =
    run.toolOutput.contains("killed") && duration < summary.wallTimeLimit &&
    (for (maxRss <- run.maxRss; memLimitKB <- summary.memLimitKB)
     yield Util.kiloBytes(maxRss) >= memoryKillRssFraction * memLimitKB)
      .getOrElse(false)

  // a nonzero exit code, written as "other error", without an error of the
  // output parser, or killed before the wall time limit without a verdict
  private def isCrash(run : RunInfoRaw, parsed : Result, duration : Double,
                      summary : Summary) : Boolean =
    run.toolOutput.exists(_.startsWith("other error")) && !parsed.isInstanceOf[Error] ||
    parsed == Timeout && run.toolOutput.contains("killed") &&
    duration < summary.wallTimeLimit

  private def printTable5TextFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    val sortedRuns = inTableOrder(runs)

    val hasIncorrect =
      !incorrectAsUnsolved && runs.exists { case (_, r) => r.incorrectRuns.nonEmpty }

    val data = sortedRuns.map { case (summary, r) =>
      val satCount = if (incorrectAsUnsolved) r.correctSatRuns.length else r.satRuns.length
      val unsatCount = if (incorrectAsUnsolved) r.correctUnsatRuns.length else r.unsatRuns.length
      val safeStr = if (hasIncorrect) s"$satCount (${r.correctSatRuns.length})" else s"$satCount"
      val unsafeStr = if (hasIncorrect) s"$unsatCount (${r.correctUnsatRuns.length})" else s"$unsatCount"
      val allCols = Seq(
        Col("tool", "Tool", summary.fullToolName),
        Col("sat", if (hasIncorrect) "Sat (corr)" else "Sat", safeStr),
        Col("unsat", if (hasIncorrect) "Unsat (corr)" else "Unsat", unsafeStr),
        Col("incorrect", "Incorrect", s"${r.incorrectRuns.length}"),
        Col("timeout", "Timeout", s"${r.timeoutRuns.length}"),
        Col("memout", "Memout", s"${r.memoutRuns.length}"),
        Col("error", "Error", s"${r.errorRuns.length - r.memoutRuns.length}"),
        Col("unknown", "Unknown", s"${r.unknownRuns.length}"),
        Col("total", "Total", s"${r.runs.length}")
      ).filter(col => incorrectAsUnsolved || col.key != "incorrect")
      filterCols(allCols)
    }

    if (data.isEmpty) return
    val header = data.head.map(_.header)
    val rows = data.map(_.map(_.value))

    val allRows = header +: rows
    val colWidths = allRows.transpose.map(col => col.map(_.length).max)

    def pad(s: String, width: Int) = s.padTo(width, ' ')

    println(header.zip(colWidths).map { case (h, w) => pad(h, w) }.mkString(" | "))
    println(colWidths.map(w => "-" * w).mkString("-|-"))

    rows.foreach { row =>
      println(row.zip(colWidths).map { case (cell, w) => pad(cell, w) }.mkString(" | "))
    }
  }

  private def printTable6TextFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    val orderedRuns = inTableOrder(runs)
    val summaries = orderedRuns.map(_._1)
    val results = orderedRuns.map(_._2)

    if (results.isEmpty) {
      println("No results to display.")
      return
    }

    val allBenchmarkNames = results.flatMap(_.runs.map(_.bmBaseName)).distinct.sorted
    val bmColWidth = (allBenchmarkNames :+ "Benchmark").map(_.length).max
    val toolHeaders = (1 to summaries.size).map(i => s"($i)").mkString(" ")
    println(s"${"Benchmark".padTo(bmColWidth, ' ')} | $toolHeaders")
    println(s"${"-" * bmColWidth}-+-${"-" * toolHeaders.length}")

    val runMap = orderedRuns.flatMap { case (summary, runInfos) =>
      runInfos.runs.map(r => (s"${summary.notes} ${summary.toolName}", r.bmBaseName) -> r)
    }.toMap

    for (bmName <- allBenchmarkNames) {
      val bmNamePadded = bmName.padTo(bmColWidth, ' ')
      val resultString = summaries.map { summary =>
        runMap.get((s"${summary.notes} ${summary.toolName}", bmName)) match {
          case Some(run) => run.result match {
            case True    => "S"
            case False   => "U"
            case Timeout => "?"
            case Error(_,_) => "E"
            case _       => "?"
          }
          case None => " "
        }
      }.map(s => s" ${s} ").mkString("")
      println(s"$bmNamePadded |$resultString")
    }

    println("\nLegend:")
    summaries.zipWithIndex.foreach { case (summary, i) =>
      println(f"(${(i + 1)}%2d): ${summary.fullToolName}")
    }
  }

  private def printTable5LatexFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    println("% For this table, please include \\usepackage{booktabs} in your LaTeX preamble.")
    val sortedRuns = inTableOrder(runs)

    val hasIncorrect =
      !incorrectAsUnsolved && runs.exists { case (_, r) => r.incorrectRuns.nonEmpty }

    val data = sortedRuns.map { case (summary, r) =>
      val toolName = sanitizeToolNameForLatex(summary.toolName)
      val satCount = if (incorrectAsUnsolved) r.correctSatRuns.length else r.satRuns.length
      val unsatCount = if (incorrectAsUnsolved) r.correctUnsatRuns.length else r.unsatRuns.length
      val safeStr = if (hasIncorrect) s"$satCount (${r.correctSatRuns.length})" else s"$satCount"
      val unsafeStr = if (hasIncorrect) s"$unsatCount (${r.correctUnsatRuns.length})" else s"$unsatCount"
      val allCols = Seq(
        Col("tool", "Tool", toolName),
        Col("sat", if (hasIncorrect) "Sat (corr)" else "Sat", safeStr),
        Col("unsat", if (hasIncorrect) "Unsat (corr)" else "Unsat", unsafeStr),
        Col("incorrect", "Incorrect", s"${r.incorrectRuns.length}"),
        Col("timeout", "Timeout", s"${r.timeoutRuns.length}"),
        Col("memout", "Memout", s"${r.memoutRuns.length}"),
        Col("error", "Error", s"${r.errorRuns.length - r.memoutRuns.length}"),
        Col("unknown", "Unknown", s"${r.unknownRuns.length}"),
        Col("total", "Total", s"${r.runs.length}")
      ).filter(col => incorrectAsUnsolved || col.key != "incorrect")
      filterCols(allCols)
    }

    if (data.isEmpty) return
    val header = data.head.map(_.header)
    val numCols = header.length

    val headerRow = header.mkString(" & ") + " \\\\ \\midrule"
    val dataRows = data.map(_.map(_.value).mkString(" & ") + " \\\\").mkString("\n")
    val colSpec = "l" + "r" * (numCols - 1)

    val latexTableString =
      s"""\\begin{table}[h]
         |  \\centering
         |  \\caption{Combined Results Summary}
         |  \\label{tbl:combined-results-summary}
         |  \\begin{tabular}{$colSpec}
         |    \\toprule
         |    $headerRow
         |    $dataRows \\\\
         |    \\bottomrule
         |  \\end{tabular}
         |\\end{table}
         |""".stripMargin
    println(latexTableString)
  }

  private def printTable6LatexFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    println("% For this table, please include the following packages in your " +
            "LaTeX preamble:\n\\usepackage{rotating}" +
            "\n\\usepackage{booktabs}\n\\usepackage{longtable}" +
            "\n\\usepackage{xcolor}\n")
    val orderedRuns = inTableOrder(runs)
    val summaries = orderedRuns.map(_._1)
    val results = orderedRuns.map(_._2)
    val numTools = summaries.size

    val headerItems = summaries.map { s =>
      val toolLabel = s"${sanitizeEncodingForLatex(s.notes)} ${sanitizeToolNameForLatex(s.toolName)}"
      s"\\rotatebox{90}{$toolLabel}"
    }
    val headerRow = "Benchmark & " + headerItems.mkString(" & ") + " \\\\ \\toprule"

    val allBenchmarkNames = results.flatMap(_.runs.map(_.bmBaseName)).distinct.sorted
    val runMap = orderedRuns.flatMap { case (summary, runInfos) =>
      runInfos.runs.map(r => (s"${summary.notes} ${summary.toolName}", r.bmBaseName) -> r)
    }.toMap

    val dataRows = allBenchmarkNames.map { bmName =>
      val benchmarkName =  {
        val n1 = bmName.replace("_", "\\_")
        if (stripSafeUnsafeSuffixInTable6)
          n1.replace("-unsafe","").replace("-safe","")
        else n1
      }
      val rowData = summaries.map { summary =>
        runMap.get(s"${summary.notes} ${summary.toolName}", bmName) match {
          case Some(run) =>
            var res = run.result match {
              case True    => "S"
              case False   => "U"
              case Timeout => "?"
              case Error(_,_) => "E"
              case _       => "?"
            }
            if (run.result == run.expected && (run.result == True || run.result == False)) {
              res = s"\\textbf{\\textcolor{green!50!black}{${res}}}"
            } else if (run.expected != Unknown && (run.result == True || run.result == False)) {
              res = s"\\underline{\\textbf{\\textcolor{red!50!black}{${res}}}}"
            }
            s" & $res"
          case None => " & "
        }
      }.mkString("")
      s"$benchmarkName$rowData \\\\"
    }.mkString("\n")

    val longtableString =
      s"""\\begin{longtable}{l${"c" * numTools}}
         |\\caption{Per benchmark results}\\label{tbl:per-benchmark-results}\\\\
         |$headerRow
         |\\endfirsthead
         |$headerRow
         |\\endhead
         |\\bottomrule
         |\\endlastfoot
         |$dataRows
         |\\end{longtable}
         |""".stripMargin
    println(longtableString)
  }

  private def printTable5SimpleTextFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    val sortedRuns = inTableOrder(runs)

    val hasIncorrect = runs.exists { case (_, r) => r.incorrectRuns.nonEmpty }

    val data = sortedRuns.map { case (summary, r) =>
      var allCols = Seq(
        Col("tool", "Tool", summary.fullToolName),
        Col("sat", "Sat", s"${r.correctSatRuns.length}"),
        Col("unsat", "Unsat", s"${r.correctUnsatRuns.length}")
      )
      if (hasIncorrect) {
        allCols = allCols :+ Col("incorrect", "Incorrect", s"${r.incorrectRuns.length}")
      }
      allCols = allCols ++ Seq(
        Col("unknown", "Unknown", s"${r.unknownRuns.length + r.errorRuns.length + r.timeoutRuns.length}"),
        Col("total", "Total", s"${r.runs.length}")
      )
      filterCols(allCols)
    }

    if (data.isEmpty) return
    val header = data.head.map(_.header)
    val rows = data.map(_.map(_.value))

    val allRows = header +: rows
    val colWidths = allRows.transpose.map(col => col.map(_.length).max)

    def pad(s: String, width: Int) = s.padTo(width, ' ')

    println(header.zip(colWidths).map { case (h, w) => pad(h, w) }.mkString(" | "))
    println(colWidths.map(w => "-" * w).mkString("-|-"))

    rows.foreach { row =>
      println(row.zip(colWidths).map { case (cell, w) => pad(cell, w) }.mkString(" | "))
    }
  }

  private def printTable5SimpleLatexFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    println("% For this table, please include \\usepackage{booktabs} in your LaTeX preamble.")
    val sortedRuns = inTableOrder(runs)

    val hasIncorrect = runs.exists { case (_, r) => r.incorrectRuns.nonEmpty }

    val data = sortedRuns.map { case (summary, r) =>
      val toolName = sanitizeToolNameForLatex(summary.toolName)
      var allCols = Seq(
        Col("tool", "Tool", toolName),
        Col("sat", "Sat", s"${r.correctSatRuns.length}"),
        Col("unsat", "Unsat", s"${r.correctUnsatRuns.length}")
      )
      if (hasIncorrect) {
        allCols = allCols :+ Col("incorrect", "Incorrect", s"${r.incorrectRuns.length}")
      }
      allCols = allCols ++ Seq(
        Col("unknown", "Unknown", s"${r.unknownRuns.length + r.errorRuns.length + r.timeoutRuns.length}"),
        Col("total", "Total", s"${r.runs.length}")
      )
      filterCols(allCols)
    }

    if (data.isEmpty) return
    val header = data.head.map(_.header)
    val numCols = header.length

    val headerRow = header.mkString(" & ") + " \\\\ \\midrule"
    val dataRows = data.map(_.map(_.value).mkString(" & ") + " \\\\").mkString("\n")
    val colSpec = "l" + "r" * (numCols - 1)

    val latexTableString =
      s"""\\begin{table}[h]
         |  \\centering
         |  \\caption{Simplified Combined Results Summary}
         |  \\label{tbl:combined-results-summary-simple}
         |  \\begin{tabular}{$colSpec}
         |    \\toprule
         |    $headerRow
         |    $dataRows \\\\
         |    \\bottomrule
         |  \\end{tabular}
         |\\end{table}
         |""".stripMargin
    println(latexTableString)
  }

  private def printMatrixTextFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    val sortedRuns = inTableOrder(runs)
    val summaries = sortedRuns.map(_._1)

    if (summaries.isEmpty) {
      println("No results to display.")
      return
    }

    val matrixData = for ((_, runsA) <- sortedRuns) yield {
      for ((_, runsB) <- sortedRuns) yield {
        if (runsA == runsB) {
          "-"
        } else {
          val diff = runsA - runsB
          s"${diff.correctSatRuns.length}/${diff.correctUnsatRuns.length}"
        }
      }
    }

    val numberedHeaders = (1 to summaries.size).map(i => s"($i)")
    val firstColHeader = " "

    val dataRows = matrixData.zipWithIndex.map { case (rowData, index) =>
      s"(${(index + 1)})" +: rowData
    }

    val allRowsAsStrings = (firstColHeader +: numberedHeaders) +: dataRows
    val colWidths = allRowsAsStrings.transpose.map(col => col.map(_.length).max)

    def pad(s: String, width: Int) = s.padTo(width, ' ')

    println(allRowsAsStrings.head.zip(colWidths).map { case (h, w) => pad(h, w) }.mkString(" | "))
    println(colWidths.map(w => "-" * w).mkString("-|-"))

    allRowsAsStrings.tail.foreach { row =>
      println(row.zip(colWidths).map { case (cell, w) => pad(cell, w) }.mkString(" | "))
    }

    println("\nLegend:")
    summaries.zipWithIndex.foreach { case (summary, i) =>
      println(f"(${(i + 1)}%2d): ${summary.fullToolName}")
    }
  }

  // per ordered pair of tools, the benchmarks solved by the first and not by
  // the second
  private def printMatrixDiffFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    val sortedRuns = inTableOrder(runs)

    if (sortedRuns.size < 2) {
      println("At least two tools are needed for -matrix-diff.")
      return
    }

    var anyFound = false
    for (((summaryA, runsA), i) <- sortedRuns.zipWithIndex;
         ((summaryB, runsB), j) <- sortedRuns.zipWithIndex if i != j) {
      val diff = runsA - runsB
      val sat = diff.correctSatRuns
      val unsat = diff.correctUnsatRuns
      if (sat.nonEmpty || unsat.nonEmpty) {
        anyFound = true
        println(s"${summaryA.fullToolName} solved but ${summaryB.fullToolName} did not " +
                s"(${sat.length} sat, ${unsat.length} unsat):")
        for ((label, group) <- Seq(("sat", sat), ("unsat", unsat)) if group.nonEmpty) {
          for (run <- group.sortBy(_.bmBaseName)) {
            val other = runsB.getRun(run.bmBaseName)
                             .map(_.result.toString).getOrElse("not run")
            println(f"  $label%-5s ${run.bmName}%s    [${summaryB.toolName}: $other]")
          }
        }
        println()
      }
    }
    if (!anyFound)
      println("Every tool solved the same benchmarks.")
  }

  private def printMatrixLatexFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    println("% For this table, please include \\usepackage{booktabs} and " +
            "\\usepackage{rotating} in your LaTeX preamble.")
    val sortedRuns = inTableOrder(runs)
    val summaries = sortedRuns.map(_._1)
    val numTools = summaries.size

    val headerItems = summaries.map { s =>
      val toolLabel = s"${sanitizeToolNameForLatex(s.toolName)}"
      s"\\rotatebox{90}{$toolLabel}"
    }
    val headerRow = " & " + headerItems.mkString(" & ") + " \\\\ \\midrule"

    val dataRows = sortedRuns.map { case (summaryA, runsA) =>
      val rowHeader = s"${sanitizeToolNameForLatex(summaryA.toolName)}"
      val rowData = sortedRuns.map { case (summaryB, runsB) =>
        if (summaryA.toolName == summaryB.toolName) {
          "---"
        } else {
          val diff = runsA - runsB
          s"${diff.correctSatRuns.length}/${diff.correctUnsatRuns.length}"
        }
      }.mkString(" & ")
      s"$rowHeader & $rowData \\\\"
    }.mkString("\n")

    val latexTableString =
      s"""\\begin{table}[h]
         |  \\centering
         |  \\caption{Comparison Matrix (Row tool solved benchmarks that Column tool could not, S/U)}
         |  \\label{tbl:comparison-matrix}
         |  \\begin{tabular}{l|${"c" * numTools}}
         |    \\toprule
         |    $headerRow
         |    $dataRows \\\\
         |    \\bottomrule
         |  \\end{tabular}
         |\\end{table}
         |""".stripMargin
    println(latexTableString)
  }

  private def printErrorsFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    val sortedRuns = inTableOrder(runs)
    var first = true
    for ((summary, r) <- sortedRuns if r.errorRuns.nonEmpty) {
      if (!first) println()
      first = false
      println(s"${summary.fullToolName} (${r.errorRuns.length} errors):")
      for (run <- r.errorRuns) {
        val err = run.result.asInstanceOf[Error]
        val typesStr = if (err.errorTypes.nonEmpty)
          err.errorTypes.mkString(", ")
        else "Unknown"
        // Take first non-empty, non-trivial line from error message as short message
        val shortMsg = err.errorMsg.linesIterator
          .map(_.trim)
          .find(l => l.nonEmpty && (l.toLowerCase.contains("error") ||
                                    l.toLowerCase.contains("exception") ||
                                    l.toLowerCase.contains("fault") ||
                                    l.toLowerCase.contains("memory") ||
                                    l.toLowerCase.contains("failed")))
          .getOrElse(err.errorMsg.linesIterator.find(_.trim.nonEmpty).getOrElse(""))
        val truncatedMsg = if (shortMsg.length > 80) shortMsg.take(77) + "..." else shortMsg
        println(s"  ${run.bmBaseName}: [$typesStr] $truncatedMsg")
      }
    }
    if (first) println("No errors found.")
  }

  private def printInconsistentFormat(
    filteredToolRuns: Seq[(Summary, RunInfos)],
    finalCommonBenchmarkNames: Set[String]
  ): Unit = {
    def runsAreConsistent(run1: RunInfo, run2: RunInfo) = {
      run1.result match {
        case True if run2.result == False => false
        case False if run2.result == True => false
        case _ => true
      }
    }
    val inconsistentRuns = new ArrayBuffer[String]
    for (bmName <- finalCommonBenchmarkNames) {
      // over several groups, a tool may have no run of the benchmark
      val runPerTool =
        filteredToolRuns.flatMap { case (summary, toolRuns) =>
          toolRuns.getRun(bmName).map(run => (summary.fullToolName, run))
        }
      for (Seq((tool1, run1), (tool2, run2)) <- runPerTool.combinations(2)) {
        if (!runsAreConsistent(run1, run2)) {
          inconsistentRuns +=
            s"${run1.bmBaseName}: expected=${run1.expected}, " +
            s"$tool1=${run1.result}, " +
            s"$tool2=${run2.result}"
        }
      }
    }
    if (inconsistentRuns.nonEmpty) {
      println(s"${inconsistentRuns.length} inconsistent result(s):")
      inconsistentRuns.sorted.foreach(s => println(s"  $s"))
    } else {
      println("No inconsistent results.")
    }
  }

  private def activePortfolios : Seq[VirtualPortfolio] =
    (if (Settings.doPortfolio) Settings.virtualPortfolios else Nil) ++
      Settings.portfoliosFromArgs

  private def isConstituent(portfolio : VirtualPortfolio, summary : Summary) : Boolean = {
    val toolMatch = portfolio.tools.contains("all") || toolNames(summary).exists(portfolio.tools.contains)
    val encodingMatch = portfolio.encodings.contains("all") || portfolio.encodings.contains(getCleanEncodingName(summary.notes))
    toolMatch && encodingMatch
  }

  /**
   * Creates virtual portfolios based on definitions in Settings.
   * A portfolio's result for a benchmark is the result of the fastest
   * constituent run that solved it.
   */
  private def createVirtualPortfolios(runs: Seq[(Summary, RunInfos)]): Seq[(Summary, RunInfos)] = {
    val portfolios = activePortfolios
    if (portfolios.isEmpty) {
      return runs
    }

    val newPortfolios = for (portfolio <- portfolios) yield {
      printInfo(s"Creating virtual portfolio: ${portfolio.name}")

      val constituentRuns = runs.filter { case (summary, _) =>
        isConstituent(portfolio, summary)
      }

      if (constituentRuns.isEmpty) {
        printWarning(s"Warning: No runs found for virtual portfolio '${portfolio.name}'. Skipping.")
        None
      } else {
        val allBenchmarkNames = constituentRuns.flatMap(_._2.runs.map(_.bmBaseName)).distinct

        val portfolioRunInfos = for (bmName <- allBenchmarkNames.toList) yield {
          val candidateRuns = constituentRuns.flatMap(_._2.getRun(bmName))

          val solvedRuns = candidateRuns.filter(r =>
            (r.result == True || r.result == False) && !(incorrectAsUnsolved && r.isIncorrect))

          val bestRun = if (solvedRuns.nonEmpty) {
            solvedRuns.minBy(_.duration)
          } else {
            // Fallback logic
            candidateRuns.find(_.result == Timeout)
                         .orElse(candidateRuns.find(_.result == Unknown))
                         .orElse(candidateRuns.find(!_.isIncorrect))
                         .orElse(candidateRuns.headOption) // Should always exist
                         .get
          }
          bestRun
        }

        val representativeSummary = constituentRuns.head._1
        val portfolioSummary = representativeSummary.copy(
          toolName = portfolioToolName,
          notes = portfolio.name,
          ymlFileName = "virtual.yml"
          )

        Some((portfolioSummary, RunInfos(portfolioRunInfos)))
      }
    }
    runs ++ newPortfolios.flatten
  }

  private val portfolioToolName = "V. Portfolio"

  // the names of the tool of a row in options: its name in the logs with and
  // without notes, for a portfolio also its name in -portfolio
  private def toolNames(summary : Summary) : Set[String] =
    Set(summary.toolName, summary.fullToolName) ++
      (if (summary.toolName == portfolioToolName) Set(summary.notes) else Nil)

  // the rows by their first name in -tools, then by name
  private def selectionOrder(summary : Summary) : (Int, String) =
    (selectedTools.indexWhere(toolNames(summary).contains), summary.fullToolName)

  /** The rows of the tools in -tools, in its order; without -tools all rows. */
  private def selectTools(runs : Seq[(Summary, RunInfos)]) : Seq[(Summary, RunInfos)] =
    if (selectedTools.isEmpty) runs
    else runs.filter(row => selectedTools.exists(toolNames(row._1).contains))
             .sortBy(row => selectionOrder(row._1))

  // the runs of each tool over all groups, a benchmark named by its group and
  // its name, so that the names in two groups are distinct
  private def unionOfGroups(groups : Seq[Group]) : Group = {
    val rows = for (group <- groups; (summary, runInfos) <- group.toolRuns)
      yield (summary, runInfos.runs.map(run =>
        run.copy(uniqueBaseName = Some(group.name + "/" + run.bmBaseName))))
    val toolRuns = rows.groupBy(_._1.fullToolName).values.toSeq
      .map(toolRows => (toolRows.head._1, RunInfos(toolRows.flatMap(_._2))))
      .sortBy(row => selectionOrder(row._1))
    Group("", toolRuns, toolRuns.flatMap(_._2.runs.map(_.bmBaseName)).toSet)
  }

  private def renamedSummary(summary : Summary) : Summary = {
    val newName = Settings.runtimeToolNameReplacements.getOrElse(
      summary.toolName, summary.toolName)
    summary.copy(toolName = newName)
  }

  // renamed only after the raw names were used for parsing, merging,
  // portfolios and the selection of tools
  private def renamed(runs : Seq[(Summary, RunInfos)]) : Seq[(Summary, RunInfos)] =
    runs.map { case (summary, runInfos) => (renamedSummary(summary), runInfos) }

  // the printed names of the rows of the group that options name by `names`
  private def printedNames(group : Group, names : Seq[String]) : Set[String] =
    group.toolRuns.map(_._1)
      .filter(summary => names.exists(toolNames(summary).contains))
      .map(summary => renamedSummary(summary).fullToolName).toSet

  // the row of the group with `name` among its names, if it is the only one;
  // not renamed
  private def namedRow(group : Group, name : String) : Option[(Summary, RunInfos)] =
    group.toolRuns.filter(row => toolNames(row._1).contains(name)) match {
      case Seq(row) => Some(row)
      case _ => None
    }

  // the printed names of the tools of each portfolio of the group, by the
  // printed name of the portfolio
  private def portfolioMembers(group : Group) : Map[String, Set[String]] = {
    val summaries = group.toolRuns.map(_._1)
    val tools = summaries.filter(_.toolName != portfolioToolName)
    (for (summary <- summaries if summary.toolName == portfolioToolName;
          portfolio <- activePortfolios.find(_.name == summary.notes))
     yield renamedSummary(summary).fullToolName ->
       tools.filter(isConstituent(portfolio, _))
         .map(renamedSummary(_).fullToolName).toSet).toMap
  }

  /** The .yml files of a directory, or the file itself. */
  private def ymlFilesOf(in : java.io.File) : List[java.io.File] = {
    val files = if (in.isDirectory) {
      printInfo("Processing all .yml files under " + in + "...")
      val fileList = in.listFiles()
      if (fileList != null) fileList.toList else List.empty
    } else {
      List(in)
    }
    files.filter(_.getName.endsWith(".yml"))
  }

  def main(args: Array[String]) : Unit = {
    val usage =
      """|Usage: yml2stats [options] (inFileName | inDirName)...
         |
         |Processes .yml files from benchmark runs and generates summary tables.
         |
         |inFileName      : A single .yml input file to process.
         |inDirName       : A directory containing .yml files to process.
         |
         |Several inputs are groups, e.g. categories, named by the last part of their paths.
         |The tables over all groups contain the runs of each tool in its groups, the benchmarks
         |prefixed by their group (GROUP/name).
         |
         |Options:
         |  -summary      : Print simplified summary table in text format.
         |  -summary-tex  : Print simplified summary table in LaTeX format.
         |  -summary-ext  : Print extended summary table in text format (was -table5tex default without tex).
         |                  Out-of-memory errors are in Memout, the other errors in Error.
         |  -summary-ext-tex : Print extended summary table in LaTeX format.
         |  -details      : Print detailed per-benchmark results in text format.
         |  -details-tex  : Print detailed per-benchmark results in LaTeX format.
         |  -matrix       : Print matrix of comparative results in text format.
         |  -matrixtex    : Print matrix of comparative results in LaTeX format.
         |  -matrix-diff  : List the benchmarks each tool solved that another did not.
         |  -errors       : Print benchmarks that had errors, with error messages.
         |  -inconsistent : Print benchmarks with inconsistent results across tools.
         |  -overview     : Print the solved benchmarks (sat/unsat) per group and tool in text format,
         |                  with the number of benchmarks of each group and a total.
         |  -overview-tex : Print the solved benchmarks per group and tool in LaTeX format, the best
         |                  values of each group in bold.
         |  -unique       : Print per group and tool the benchmarks (sat/unsat) solved by the tool and by
         |                  no other tool in text format; a portfolio is compared with the tools that
         |                  are not its members, and portfolios are no other tools.
         |  -unique-tex   : Print the uniquely solved benchmarks per group and tool in LaTeX format.
         |  -no-best:TOOL1,TOOL2,... : Leave these tools out of the best values of -overview-tex.
         |  -midrule-after:GROUP : A \midrule after the row of GROUP in -overview-tex and -unique-tex
         |                  (repeatable).
         |  -compare:TOOL,OTHER : Print over all groups the solved benchmarks and memouts of TOOL and of
         |                  OTHER and the benchmarks (sat/unsat) solved only by TOOL and only by OTHER in
         |                  text format (repeatable, one row per pair).
         |  -compare-tex:TOOL,OTHER : The same in LaTeX format (repeatable).
         |  -runtimes:BASE : Print over all groups per tool other than BASE and verdict (sat, unsat) the
         |                  benchmarks solved by both with the verdict, those solved in less and in more time
         |                  by the tool, and the median and the geometric mean of the time ratio tool/BASE
         |                  in text format (repeatable, one table per BASE).
         |  -runtimes-tex:BASE : The same in LaTeX format (repeatable).
         |  -errors-as-errors : Exclude error benchmarks from comparisons (don't treat them as unknown).
         |  -cactus-pdf   : Generate a cactus plot in PDF format.
         |  -cactus-plotly: Generate a cactus plot using Plotly (DISABLED).
         |  -v            : Set verbosity level to 1 (warnings).
         |  -v:N          : Set verbosity level to N (0=quiet, 1=warnings, 2=info).
         |  -rename-tool:FROM=TO  : Rename a tool in all output; -portfolio and file merging use FROM (repeatable).
         |  -rename-header:FROM=TO : Rename a column header (repeatable).
         |  -hide-col:NAME : Hide a column by key: sat, unsat, incorrect, timeout, memout, error, unknown, total (repeatable).
         |  -portfolio:NAME=TOOL1,TOOL2,... : Add the virtual portfolio NAME of the tools: per benchmark
         |                  the fastest run of the tools that solved it (repeatable).
         |  -tools:TOOL1,TOOL2,... : Only these tools and portfolios (NAME of -portfolio), in this order
         |                  in every table; the other tools are left out before the comparisons.
         |  -expected:FILE : Expected verdicts from FILE in place of those in the logs: per line the name of a
         |                  benchmark as in the logs and its verdict, e.g. sat or unsat (repeatable).
         |  -incorrect-as-unsolved : An incorrect verdict, which contradicts the expected verdict, is
         |                  unsolved: in the column Incorrect of the extended summary, not in Sat or Unsat;
         |                  a portfolio solves a benchmark only with a correct verdict.
         |  -crashes-as-errors : A run that ended with a nonzero exit code (other error) or was killed before
         |                  the wall time limit without a verdict and not for its memory is an error.
         |  -unknown-as-timeout:N : A run with the result unknown that lasted at least N seconds is a timeout,
         |                  e.g. for a tool that answers unknown at its own time limit.
         |
         |Default (no options): Print the summary table in text format.
         |""".stripMargin

    val arglist = args.toList
    if (args.length == 0 || arglist.contains("-h") || arglist.contains("--help")) {
      println(usage); return
    }

    var remainingArgs = arglist
      while (remainingArgs.nonEmpty) {
        remainingArgs match {
          case "-summary" :: tail =>
            doTable5SimpleText = true
            remainingArgs = tail
          case "-summary-tex" :: tail =>
            doTable5SimpleTex = true
            remainingArgs = tail
          case "-summary-ext" :: tail => // old doTable5Text
            doTable5Text = true
            remainingArgs = tail
          case "-summary-ext-tex" :: tail =>
            doTable5Tex = true
            remainingArgs = tail
          case "-details-tex" :: tail =>
            doTable6Tex = true
            remainingArgs = tail
          case "-details" :: tail =>
            doTable6Text = true
            remainingArgs = tail
        case "-matrix-diff" :: tail =>
          doMatrixDiff = true
          remainingArgs = tail
        case "-matrix" :: tail =>
          doMatrixText = true
          remainingArgs = tail
        case "-matrixtex" :: tail =>
          doMatrixTex = true
          remainingArgs = tail
        case "-errors" :: tail =>
          doErrors = true
          remainingArgs = tail
        case "-inconsistent" :: tail =>
          doInconsistent = true
          remainingArgs = tail
        case "-errors-as-errors" :: tail =>
          excludeErrors = true
          remainingArgs = tail
        case "-cactus-plotly" :: tail =>
          doCactusPlotly = true
          remainingArgs = tail
        case "-cactus-pdf" :: tail =>
          doCactusPdf = true
          remainingArgs = tail
        case "-v" :: tail =>
          verbosityLevel = 1
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-v:") =>
          try {
            verbosityLevel = opt.substring(3).toInt
          } catch {
            case _: NumberFormatException =>
              println(s"Invalid verbosity level in option: $opt")
              println(usage)
              return
          }
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-rename-tool:") =>
          val spec = opt.substring("-rename-tool:".length)
          val eqIdx = spec.indexOf('=')
          if (eqIdx > 0) {
            Settings.runtimeToolNameReplacements(spec.substring(0, eqIdx)) = spec.substring(eqIdx + 1)
          }
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-rename-header:") =>
          val spec = opt.substring("-rename-header:".length)
          val eqIdx = spec.indexOf('=')
          if (eqIdx > 0) {
            Settings.headerRenames(spec.substring(0, eqIdx)) = spec.substring(eqIdx + 1)
          }
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-hide-col:") =>
          Settings.hiddenColumns += opt.substring("-hide-col:".length).toLowerCase
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-portfolio:") =>
          val spec = opt.substring("-portfolio:".length)
          val eqIdx = spec.indexOf('=')
          val tools = spec.substring(eqIdx + 1).split(",").map(_.trim)
                          .filter(_.nonEmpty).toSet
          if (eqIdx <= 0 || tools.isEmpty) {
            println(s"Invalid portfolio in option: $opt\n")
            println(usage)
            return
          }
          Settings.portfoliosFromArgs +=
            VirtualPortfolio(spec.substring(0, eqIdx), tools, Set("all"))
          remainingArgs = tail
        case "-overview" :: tail =>
          doOverviewText = true
          remainingArgs = tail
        case "-overview-tex" :: tail =>
          doOverviewTex = true
          remainingArgs = tail
        case "-unique" :: tail =>
          doUniqueText = true
          remainingArgs = tail
        case "-unique-tex" :: tail =>
          doUniqueTex = true
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-compare:") || opt.startsWith("-compare-tex:") =>
          opt.substring(opt.indexOf(':') + 1).split(",").map(_.trim) match {
            case Array(tool, other) if opt.startsWith("-compare:") =>
              comparisonsText :+= ((tool, other))
            case Array(tool, other) =>
              comparisonsTex :+= ((tool, other))
            case _ =>
              println(s"Invalid pair of tools in option: $opt\n")
              println(usage)
              return
          }
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-runtimes:") =>
          runtimeBasesText :+= opt.substring("-runtimes:".length).trim
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-runtimes-tex:") =>
          runtimeBasesTex :+= opt.substring("-runtimes-tex:".length).trim
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-no-best:") =>
          noBestTools ++= opt.substring("-no-best:".length).split(",")
                             .map(_.trim).filter(_.nonEmpty)
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-midrule-after:") =>
          midruleAfterGroups += opt.substring("-midrule-after:".length)
          remainingArgs = tail
        case "-incorrect-as-unsolved" :: tail =>
          incorrectAsUnsolved = true
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-unknown-as-timeout:") =>
          try {
            unknownAsTimeoutAfter =
              Some(opt.substring("-unknown-as-timeout:".length).toDouble)
          } catch {
            case _: NumberFormatException =>
              println(s"Invalid number of seconds in option: $opt")
              println(usage)
              return
          }
          remainingArgs = tail
        case "-crashes-as-errors" :: tail =>
          crashesAsErrors = true
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-expected:") =>
          expectedVerdicts ++= Util.readExpectedVerdicts(opt.substring("-expected:".length))
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-tools:") =>
          selectedTools = opt.substring("-tools:".length).split(",")
                             .map(_.trim).filter(_.nonEmpty).toSeq
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-") =>
          println(s"Unknown option: $opt\n")
          println(usage)
          return
        case path :: tail =>
          inFileNames += path
          remainingArgs = tail
      }
    }

    if (!doTable5Tex && !doTable6Tex && !doTable6Text && !doTable5SimpleText && !doTable5SimpleTex && !doMatrixText && !doMatrixTex && !doMatrixDiff && !doErrors && !doInconsistent && !doCactusPdf && !doCactusPlotly &&
        !doOverviewText && !doOverviewTex && !doUniqueText && !doUniqueTex &&
        comparisonsText.isEmpty && comparisonsTex.isEmpty &&
        runtimeBasesText.isEmpty && runtimeBasesTex.isEmpty) {
      doTable5Text = true
    }

    if (inFileNames.isEmpty) {
      println("An input filename must be provided.\n")
      println(usage)
      return
    }

    inFileNames.find(name => !new java.io.File(name).exists) match {
      case Some(inFileName) =>
        println(inFileName + " not found!"); return
      case None =>
    }

    val ymlFilesOfInputs =
      inFileNames.toList.map(name => (name, ymlFilesOf(new java.io.File(name))))

    ymlFilesOfInputs.find(_._2.isEmpty) match {
      case Some((inFileName, _)) =>
        println("No .yml files found in " + inFileName)
        return
      case None =>
    }

    val groupNames = ymlFilesOfInputs.map(input => new java.io.File(input._1).getName)
    groupNames.diff(groupNames.distinct).headOption match {
      case Some(name) =>
        println("Several inputs with the name " + name + ", the name of a group.")
        return
      case None =>
    }
    val groups = for (((_, ymlFiles), name) <- ymlFilesOfInputs.zip(groupNames))
      yield readGroup(name, ymlFiles)
    for (name <- selectedTools
         if !groups.exists(_.toolRuns.exists(row => toolNames(row._1) contains name)))
      printWarning(s"Warning: No runs found for the tool '$name'.")
    val allGroups = if (groups.size == 1) groups.head else unionOfGroups(groups)
    val unnamed = ((comparisonsText ++ comparisonsTex)
      .flatMap(pair => Seq(pair._1, pair._2)) ++
      runtimeBasesText ++ runtimeBasesTex).distinct
      .filter(name => namedRow(allGroups, name).isEmpty)
    if (unnamed.nonEmpty) {
      println("No single tool with runs named " + unnamed.mkString(", ") + ".")
      return
    }
    printOutputs(groups, allGroups)
  }

  // the runs of the tools of the yml files: the files of a tool merged,
  // portfolios added and tools selected
  private[yml2stats] def readGroup(name : String, ymlFiles : List[java.io.File]) : Group = {
////////////////////////////////////////////////////////////////////////////////
// Read YAML files and convert into useful data structures (one file at a time
// to avoid keeping all raw YAML data in memory simultaneously)

    val loadedToolRuns : Seq[(Summary, Seq[RunInfo])] =
      for (file <- ymlFiles) yield {
        val fileName = file.getName
        printInfo("Processing " + fileName + "...")

        val inFile = Source.fromFile(file)
        val source = inFile.getLines.mkString("\n")
        inFile.close

        val ast = try {
          source.parseYaml
        } catch {
          case _ : Throwable =>
            throw new Exception("Could not parse " + fileName)
        }

        val (rawSummary, rawRunInfos) =
          ast.convertTo[(SummaryRaw, Seq[RunInfoRaw])]

        val outputParser: ToolOutputParser =
          rawSummary.toolName match {
            case s if s.toLowerCase contains "eld"       => EldaricaOutputParser
            case s if s.toLowerCase contains "z3"        => Z3OutputParser
            case s if s.toLowerCase contains "cpa"       => CPAOutputParser
            case s if s.toLowerCase contains "sea"       => SeaHornOutputParser
            case s if s.toLowerCase contains "tri"       => TriCeraOutputParser
            case s if s.toLowerCase contains "predator"  => SVOutputParser
            case _ =>
              // Check if benchmarks are SMT files
              if (rawRunInfos.exists(_.bmName.endsWith(".smt2"))) {
                printInfo("Using Standard (Z3-like) SMT Output Parser based on .smt2 extension.")
                StandardSMTOutputParser
              } else {
                 throw new Exception("An output parser for the tool " + rawSummary.toolName + " can not be determined.")
              }
          }

        val expectedStatusParser =
          rawSummary.toolName match {
            case s if (s.toLowerCase contains "eld") || (s.toLowerCase contains "z3") =>
              SMTExpectedStatusParser
            case s if (s.toLowerCase contains "tri") ||
                      (s.toLowerCase contains "sea") ||
                      (s.toLowerCase contains "predator") ||
                      (s.toLowerCase contains "cpa") =>
              TriCeraExpectedStatusParser
            case _ =>
               if (rawRunInfos.exists(_.bmName.endsWith(".smt2"))) {
                 SMTExpectedStatusParser
               } else {
                 // default fallback
                 TriCeraExpectedStatusParser
               }
          }

        val summary = Summary(rawSummary, fileName)
        val runInfos = RunInfos(for (rawRunInfo <- rawRunInfos) yield {
          val duration = rawRunInfo.duration.dropRight(1).toDouble // todo properly parse duration
          val result =
            outputParser(rawRunInfo.toolOutput, rawRunInfo.bmName) match {
              case Timeout if isMemoryKill(rawRunInfo, duration, summary) =>
                Error(Set(ErrorType.OutOfMemory),
                      rawRunInfo.toolOutput.mkString("\n"))
              case parsed if crashesAsErrors &&
                             isCrash(rawRunInfo, parsed, duration, summary) =>
                Error(Set(ErrorType.Other), rawRunInfo.toolOutput.mkString("\n"))
              case Unknown if unknownAsTimeoutAfter.exists(duration >= _) =>
                Timeout
              case parsed => parsed
            }
          val expected = expectedVerdicts.getOrElse(
            rawRunInfo.bmName, expectedStatusParser(rawRunInfo.expected))
          RunInfo(rawRunInfo.bmName, expected, result, duration)
        })
        printInfo("done! " + runInfos.length + " runs found.")
        (summary, runInfos.runs)
      }

    // unique names from the benchmarks of all files, so that a benchmark has
    // the same name in every file
    val uniqueBaseNames : Map[String, String] =
      makeUniqueBaseNames(loadedToolRuns.flatMap(_._2).groupBy(_.bmName)
                            .values.map(_.head).toSeq)
        .map(run => run.bmName -> run.bmBaseName).toMap
    val unmergedToolRuns : Seq[(Summary, Seq[RunInfo])] =
      for ((summary, runs) <- loadedToolRuns) yield
        (summary, runs.map(run =>
          run.copy(uniqueBaseName = Some(uniqueBaseNames(run.bmName)))))

    if (mergeYmlFiles && combineResults)
      printWarning("Cannot enable both mergeYmlFiles and combineResults! " +
        "Defaulting to merging.")

    def combineRuns(runs : Seq[(RunInfo, Date)]) : (RunInfo, Date) = {
      //   - if a benchmark is ERROR   in any of the combined results, result will be ERROR
      //   - if a benchmark is TIMEOUT in any of the combined results, result will be TIMEOUT
      //   - if a benchmark is UNSAT   in any of the combined results, result will be UNSAT
      //   - if a benchmark is SAT     in all of the combined results, result will be SAT
      //   - if a benchmark is SAT/UNKNOWN in all of the com. results, result will be UNKNOWN
      runs.find(run => run._1.result.isInstanceOf[Error]) match {
        case Some(run) => run
        case _ => runs.find(run => run._1.result == Timeout) match {
          case Some(run) => run
          case _ => runs.find(run => run._1.result == False) match {
            case Some(run) => run
            case _ if runs.forall(run => run._1.result == True) =>
              runs.head
            case _ if runs.forall(run => run._1.result == Unknown || run._1.result == True) =>
              runs.find(run => run._1.result == Unknown).get
            case _ =>
              throw new Exception("Cannot combine runs!" + runs.map(run =>
                run._1.result))
          }
        }
      }
    }

    val toolRunsWithoutVP : Seq[(Summary, RunInfos)] = if(mergeYmlFiles || combineResults) {
      println
      printWarning("Merging files with same tool name and options...")
      val groupedToolRuns =
        if (ignoreDifferentOptions) {
          unmergedToolRuns.groupBy(p => p._1.fullToolName)
        } else {
          unmergedToolRuns.groupBy(p =>
            if(ignoreDifferentOptionsForTools contains p._1.toolName)
              p._1.fullToolName
            else
              s"${p._1.fullToolName} (${p._1.toolOptions})")
        }
      for ((nameAndOpts, toBeMergedRuns) <- groupedToolRuns.toSeq) yield {
        // checks to ensure merged files do not differ in any parameters
        if (toBeMergedRuns.length > 1) {
          printWarning("\n\tFound " + toBeMergedRuns.length + " file(s) for " + nameAndOpts)
          val summaries = toBeMergedRuns.map(_._1)
          checkIfSameParameters(summaries)
          val summary = toBeMergedRuns.head._1 // take the summary of the first one
          val allRunsWithDate = toBeMergedRuns.flatMap(p => p._2.zip(
            p._2.indices.map(_ => p._1.startDate)))

          val flatRuns = toBeMergedRuns.flatMap(_._2)
          // there might be multiple bms with the same basename, disambiguate before proceeding
          val uniqueRunsSeq = makeUniqueBaseNames(flatRuns)
          
          val runsGroupedByBmName: Seq[(String, Seq[(RunInfo, Date)])] =
            uniqueRunsSeq.map(r => r -> toBeMergedRuns.head._1.startDate).groupBy(_._1.bmBaseName).toSeq 
          val allRunsWithDate2 = toBeMergedRuns.flatMap(p => 
             p._2.map(r => (r, p._1.startDate))
          )
          
          val uniqueRunsSeq2 = makeUniqueBaseNames(allRunsWithDate2.map(_._1))
          val runsWithDateUnique = uniqueRunsSeq2.zip(allRunsWithDate2.map(_._2))
          val runsGroupedByBmName2 = runsWithDateUnique.groupBy(_._1.bmBaseName).toSeq

          val uniqueRuns : Seq[RunInfo] =
            for ((name, runsWithDate) <- runsGroupedByBmName2) yield {
            if(runsWithDate.length > 1) {
              val resultRun : (RunInfo, Date) =
                if (mergeYmlFiles)
                  runsWithDate.maxBy(p => p._2)
                else
                  combineRuns(runsWithDate)
              printInfo("\tFound " + runsWithDate.length + " benchmarks with the same" +
                " name (" + name + ") while merging. \n" +
                "\t\tTaking the one executed on " + resultRun._2 + ".")
              resultRun._1
            } else runsWithDate.head._1
          }

          val runs = RunInfos(uniqueRuns)

          printWarning("\tMerged " + nameAndOpts + ". New total: " + runs.length + " benchmarks.")
          (summary, runs)
        } else {
           val (sum, rSeq) = toBeMergedRuns.head
           (sum, RunInfos(makeUniqueBaseNames(rSeq)))
        }
      }
    }
    else {
      unmergedToolRuns.map { case (sum, rSeq) =>
        (sum, RunInfos(makeUniqueBaseNames(rSeq)))
      }
    }.toSeq

    val toolRuns = selectTools(createVirtualPortfolios(toolRunsWithoutVP.toSeq))
    if (toolRuns.isEmpty)
      return Group(name, Nil, Set.empty)

////////////////////////////////////////////////////////////////////////////////
// Fairness checks
    if(printFairnessWarnings) {
      println
      val summaries = toolRuns.map(_._1)
      if (summaries.exists(s => s.cpuCount != summaries.head.cpuCount)) {
        printWarning("Runs were executed on systems with different CPU counts!")
        summaries.foreach(summary => printInfo("\t" + summary.fullToolName + ": " +
                                               summary.cpuCount + " (" + summary.ymlFileName + ")"))
      }
      if (summaries.exists(s => s.architecture != summaries.head.architecture)) {
        printWarning("Runs were executed on systems with different architectures!")
        summaries.foreach(summary => printInfo("\t" + summary.fullToolName + ": " +
                                               summary.architecture + " (" + summary.ymlFileName + ")"))
      }
      if (summaries.exists(s => s.cpuModel != summaries.head.cpuModel)) {
        printWarning("Runs were executed on systems with different cpu models!")
        summaries.foreach(summary => printInfo("\t" + summary.fullToolName + ": " +
                                               summary.cpuModel + " (" + summary.ymlFileName + ")"))
      }
      if (summaries.exists(s => s.memTotal != summaries.head.memTotal)) {
        printWarning("Runs were executed on systems with different total memories!")
        summaries.foreach(summary => printInfo("\t" + summary.fullToolName + ": " +
                                               summary.memTotal + " (" + summary.ymlFileName + ")"))
      }
      if (summaries.exists(s => s.wallTimeLimit != summaries.head.wallTimeLimit)) {
        printWarning("Runs were executed on systems with different wall time limits!")
        summaries.foreach(summary => printInfo("\t" + summary.fullToolName + ": " +
                                               summary.wallTimeLimit + " (" + summary.ymlFileName + ")"))
      }
      if (summaries.exists(s => s.cpuTimeLimit != summaries.head.cpuTimeLimit)) {
        printWarning("Runs were executed on systems with different CPU time limits!")
        summaries.foreach(summary => printInfo("\t" + summary.fullToolName + ": " +
                                               summary.cpuTimeLimit + " (" + summary.ymlFileName + ")"))
      }
    }

    if (printIndividualStats) {
      for ((summary, runs) <- toolRuns) {
        println(summary)
        println(runs)
        println
      }
    }

    val smallestRuns = toolRuns.minBy(pair => pair._2.length)

    val toolRunNameSets: Iterable[Set[String]] = toolRuns.map {
      case (_, tool) =>
        tool.runs.iterator.map(_.bmBaseName).toSet
    }

    val smallestRunNames: Set[String] =
      smallestRuns._2.runs.iterator.map(_.bmBaseName).toSet

    val commonBenchmarkNames: Set[String] =
      smallestRunNames.filter(name => toolRunNameSets.forall(_.contains(name)))

    printInfo(commonBenchmarkNames.size + " benchmarks were executed by all tools.")

    val errorRunNamesForEachTool : Seq[Seq[String]] =
      toolRuns.map(p => p._2.errorRuns.filter(run =>
                                                if(excludeSolverErrors)
                                                  !run.result.asInstanceOf[Error].errorTypes.contains(ErrorType.Solve)
                                                else
                                                  true
                                              ).
                         map(_.bmBaseName)).toSeq
    val combinedErrorRuns : Set[String] =
      if (errorRunNamesForEachTool.nonEmpty) errorRunNamesForEachTool.reduce(_ union _).toSet else Set.empty
    val commonBenchmarkNamesWithoutErrors : Set[String] =
      commonBenchmarkNames diff combinedErrorRuns

    printInfo(commonBenchmarkNamesWithoutErrors.size +
              " benchmarks had no errors in any of the tools.")

    val commonBenchmarkNamesMaybeWithoutErrors =
      if(excludeErrors) commonBenchmarkNamesWithoutErrors
      else commonBenchmarkNames

    printInfo(
      (if(excludeErrors) "Excluding" else "Including") +
      " benchmarks that any tool reported an error for in comparisons.\n"
      )

    val incorrectRunNamesForEachTool : Seq[Seq[String]] =
      toolRuns.map(p => p._2.incorrectRuns.map(_.bmBaseName)).toSeq
    val combinedIncorrectRuns : Set[String] =
      if(incorrectRunNamesForEachTool.nonEmpty) incorrectRunNamesForEachTool.reduce(_ union _).toSet else Set.empty
    val commonBenchmarkNamesWithoutIncorrect : Set[String] =
      commonBenchmarkNamesMaybeWithoutErrors diff combinedIncorrectRuns

    printInfo(commonBenchmarkNamesWithoutIncorrect.size +
              " benchmarks had no incorrect results in any of the tools.")
    printInfo(
      (if(excludeIncorrect) "Excluding" else "Including") +
      " benchmarks that any tool returned an incorrect result for in comparisons.\n"
      )

    val finalCommonBenchmarkNames =
      if(excludeIncorrect) commonBenchmarkNamesWithoutIncorrect
      else commonBenchmarkNamesMaybeWithoutErrors

    val filteredToolRuns : Seq[(Summary, RunInfos)] =
      (for ((summary, runs) <- toolRuns) yield {
        val filteredRuns = runs.runs.filter(run =>
                                              finalCommonBenchmarkNames contains run.bmBaseName
                                            )
        (summary, RunInfos(filteredRuns))
      }).toSeq

    Group(name, filteredToolRuns, finalCommonBenchmarkNames)
  }

  /**
   * Prints the outputs of the options.
   * @param allGroups the groups, or with several groups their union
   */
  private def printOutputs(groups : Seq[Group], allGroups : Group) : Unit = {
    val filteredToolRuns = renamed(allGroups.toolRuns)
    val finalCommonBenchmarkNames = allGroups.benchmarkNames

    if (printCombinatorialResults) {
      println
      for (((summary, runs), i) <- filteredToolRuns.zipWithIndex) {
        var uniqueDiffRuns = runs
        for (j <- filteredToolRuns.indices if i != j) {
          val diffRuns = runs - filteredToolRuns(j)._2
          println(summary.fullToolName + " solved " + diffRuns.satRuns.length + "/" +
                  diffRuns.unsatRuns.length + " that " + filteredToolRuns(j)._1.fullToolName + " could not solve.")
          if (diffRuns.satRuns.nonEmpty) {
            println("  sat")
            diffRuns.satRuns.foreach { run =>
              println(s"    ${run.bmBaseName}")
            }
          }
          if (diffRuns.unsatRuns.nonEmpty) {
            println("  unsat")
            diffRuns.unsatRuns.foreach { run =>
              println(s"    ${run.bmBaseName}")
            }
          }
          uniqueDiffRuns = uniqueDiffRuns - filteredToolRuns(j)._2
        }
        println(summary.fullToolName + " solved " + uniqueDiffRuns.satRuns.length + "/" +
        uniqueDiffRuns.unsatRuns.length + " that any other tool could not solve.")
        println
      }
    }

    var firstTable = true
    def printSep() = {
      if (!firstTable) println("\n")
      firstTable = false
    }

    if (doTable5SimpleText) {
      printSep()
      printTable5SimpleTextFormat(filteredToolRuns)
    }
    if (doTable5SimpleTex) {
      printSep()
      printTable5SimpleLatexFormat(filteredToolRuns)
    }
    if (doTable5Text) {
      printSep()
      printTable5TextFormat(filteredToolRuns)
    }
    if (doTable6Text) {
      printSep()
      printTable6TextFormat(filteredToolRuns)
    }
    if (doTable5Tex) {
      printSep()
      printTable5LatexFormat(filteredToolRuns)
    }
    if (doTable6Tex) {
      printSep()
      printTable6LatexFormat(filteredToolRuns)
    }
    if (doMatrixText) {
      printSep()
      printMatrixTextFormat(filteredToolRuns)
    }
    if (doMatrixDiff) {
      printSep()
      printMatrixDiffFormat(filteredToolRuns)
    }
    if (doMatrixTex) {
      printSep()
      printMatrixLatexFormat(filteredToolRuns)
    }
    if (doErrors) {
      printSep()
      printErrorsFormat(filteredToolRuns)
    }
    if (doInconsistent) {
      printSep()
      printInconsistentFormat(filteredToolRuns, finalCommonBenchmarkNames)
    }

    val renamedGroups =
      groups.map(group => group.copy(toolRuns = renamed(group.toolRuns)))
    val tools = inTableOrder(filteredToolRuns).map(_._1)
    val noBest = printedNames(allGroups, noBestTools)
    val members = portfolioMembers(allGroups)
    if (doOverviewText) {
      printSep()
      GroupTables.printOverview(renamedGroups, tools, noBest, latex = false)
    }
    if (doOverviewTex) {
      printSep()
      GroupTables.printOverview(renamedGroups, tools, noBest, latex = true)
    }
    if (doUniqueText) {
      printSep()
      GroupTables.printUnique(renamedGroups, tools, members, latex = false)
    }
    if (doUniqueTex) {
      printSep()
      GroupTables.printUnique(renamedGroups, tools, members, latex = true)
    }
    def renamedRow(name : String) = {
      val (summary, runs) = namedRow(allGroups, name).get
      (renamedSummary(summary), runs)
    }
    def namedRows(pairs : Seq[(String, String)]) =
      for ((tool, other) <- pairs) yield (renamedRow(tool), renamedRow(other))
    if (comparisonsText.nonEmpty) {
      printSep()
      GroupTables.printComparison(namedRows(comparisonsText), latex = false)
    }
    if (comparisonsTex.nonEmpty) {
      printSep()
      GroupTables.printComparison(namedRows(comparisonsTex), latex = true)
    }
    def printRuntimes(base : String, latex : Boolean) = {
      printSep()
      val baseRow = renamedRow(base)
      GroupTables.printRuntimes(baseRow, inTableOrder(filteredToolRuns)
        .filter(_._1.fullToolName != baseRow._1.fullToolName), latex)
    }
    runtimeBasesText.foreach(printRuntimes(_, latex = false))
    runtimeBasesTex.foreach(printRuntimes(_, latex = true))

    for ((summary, runs) <- filteredToolRuns) {
      if (runs.incorrectRuns.nonEmpty) {
        printInfo("\nIncorrect results for " + summary.fullToolName)
      }
      if(runs.unsoundRuns.nonEmpty) {
        printInfo("\nUnsound (expected unsat, got sat)")
        runs.unsoundRuns.foreach(run => printInfo(s"${run.bmName} (${run.duration} s)"))
        println
      }
      if (runs.incompleteRuns.nonEmpty) {
        printInfo("\nIncomplete (expected sat, got unsat)")
        runs.incompleteRuns.foreach(run => printInfo(s"${run.bmName} (${run.duration} s)"))
        println
      }
    }

    if (verbosityLevel > 0) {
      printInfo("Running consistency checks, this might take a while...")
      def runsAreConsistent (run1 : RunInfo, run2 : RunInfo) = {
        run1.result match {
          case True if run2.result == False => false
          case False if run2.result == True => false
          case _ => true
        }
      }
      val inconsistentRuns = new ArrayBuffer[String]
      for (bmName <- finalCommonBenchmarkNames) {
        val runPerTool =
          filteredToolRuns.flatMap{case (summary, toolRuns) =>
            toolRuns.getRun(bmName).map(run => (summary.fullToolName, run))}
        for (Seq((tool1, run1), (tool2, run2)) <- runPerTool.combinations(2)) {
          if(!runsAreConsistent(run1, run2)) {
            assert(run1.expected == run2.expected)
            inconsistentRuns +=
              s"${run1.bmName} (exp: ${run1.expected}, " +
              s"$tool1: ${run1.result}, " +
              s"$tool2: ${run2.result})"
          }
        }
      }
      if(inconsistentRuns.length > 0) {
        printWarning("\nWarning: detected " + inconsistentRuns.length + " inconsistent runs!")
        inconsistentRuns.foreach{printWarning}
      } else
        printInfo("\nNo inconsistent runs detected!")
    }

    if(printCombinatorialResults) {
      println
      for (((summary, runs), i) <- filteredToolRuns.zipWithIndex) {
        var uniqueDiffRuns = runs
        for (j <- filteredToolRuns.indices if i != j) {
          val diffRuns = runs - filteredToolRuns(j)._2
          println(summary.fullToolName + " solved " + diffRuns.satRuns.length + "/" +
                  diffRuns.unsatRuns.length + " that " +
                  filteredToolRuns(j)._1.fullToolName + " could not solve.")
          if (diffRuns.satRuns.nonEmpty) {
            println("  sat")
            diffRuns.satRuns.foreach{run =>
              println(s"    ${run.bmBaseName}")
            }
          }
          if (diffRuns.unsatRuns.nonEmpty) {
            println("  unsat")
            diffRuns.unsatRuns.foreach{run =>
              println(s"    ${run.bmBaseName}")
            }
          }
          uniqueDiffRuns = uniqueDiffRuns - filteredToolRuns(j)._2
        }
        println(summary.fullToolName + " solved " + uniqueDiffRuns.satRuns.length + "/" +
                uniqueDiffRuns.unsatRuns.length + " that any other tool could not solve.")
        println
      }
    }

    if(!disableAllPlots && (plotDurations || plotDurationsFile) &&
       filteredToolRuns.length > 1) {
      println
      println("Generating durations plots")

      val toolRunsWithExpectedInfo = if (plotDurationsUseResultInsteadOfExpected) {
        filteredToolRuns.maxBy(_._2.correctRuns.size)
      } else {
        filteredToolRuns.find(_._2.runs.exists(
          r => r.expected != Unknown)).getOrElse(filteredToolRuns.head)
      }

      def isExpectedSat(run : RunInfo) : Boolean = {
        val comp = if (plotDurationsUseResultInsteadOfExpected) run.result else run.expected
        comp == True
      }
      def isExpectedUnsat(run : RunInfo) : Boolean = {
        val comp = if (plotDurationsUseResultInsteadOfExpected) run.result else run.expected
        comp == False
      }
      def isExpectedUnknown(run : RunInfo) : Boolean = {
        val comp = if (plotDurationsUseResultInsteadOfExpected) run.result else run.expected
        comp == Unknown
      }

      val expSatNames =  toolRunsWithExpectedInfo._2.runs.filter(isExpectedSat).map(_.bmBaseName)
      val expUnsatNames = toolRunsWithExpectedInfo._2.runs.filter(isExpectedUnsat).map(_.bmBaseName)
      val expUnknownNames = toolRunsWithExpectedInfo._2.runs.filter(isExpectedUnknown).map(_.bmBaseName)

      for (Seq(toolRuns1, toolRuns2) <- filteredToolRuns.combinations(2)) {
        ???
//        Plotting.plotDuratıons(
//          toolRuns1, toolRuns2, expSatNames, expUnsatNames, expUnknownNames)
      }
    }

    if (!disableAllPlots && doCactusPdf) {
      println
      Plotting.plotCactusByTime(filteredToolRuns)
    }
    
    if (!disableAllPlots && doCactusPlotly) {
      println()
      printWarning("Cactus plot generation with Plotly is currently disabled/not implemented.")
    }
  }

  private def checkIfSameParameters(summaries: Seq[Summary]) = {
    if (summaries.exists(s => s.cpuCount != summaries.head.cpuCount)) {
      printWarning("\t\tRuns were executed on systems with different CPU counts!")
      summaries.foreach(summary => printInfo("\t" + summary.fullToolName + "(" +
                                             summary.toolVersion + ")" + " on " + summary.startDate + ": " +
                                             summary.cpuCount + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.architecture != summaries.head.architecture)) {
      printWarning("\t\tRuns were executed on systems with different architectures!")
      summaries.foreach(summary => printInfo("\t" + summary.fullToolName + "(" +
                                             summary.toolVersion + ")" + " on " + summary.startDate + ": " +
                                             summary.architecture + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.cpuModel != summaries.head.cpuModel)) {
      printWarning("\t\tRuns were executed on systems with different cpu models!")
      summaries.foreach(summary => printInfo("\t" + summary.fullToolName + "(" +
                                             summary.toolVersion + ")" + " on " + summary.startDate + ": " +
                                             summary.cpuModel + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.memTotal != summaries.head.memTotal)) {
      printWarning("\t\tRuns were executed on systems with different total memories!")
      summaries.foreach(summary => printInfo("\t" + summary.fullToolName + "(" +
                                             summary.toolVersion + ")" + " on " + summary.startDate + ": " +
                                             summary.memTotal + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.wallTimeLimit != summaries.head.wallTimeLimit)) {
      printWarning("\t\tRuns were executed on systems with different wall time limits!")
      summaries.foreach(summary => printInfo("\t" + summary.fullToolName + ": " +
                                             summary.wallTimeLimit + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.cpuTimeLimit != summaries.head.cpuTimeLimit)) {
      printWarning("\t\tRuns were executed on systems with different CPU time limits!")
      summaries.foreach(summary => printInfo("\t" + summary.fullToolName + "(" +
                                             summary.toolVersion + ")" + " on " + summary.startDate + ": " +
                                             summary.cpuTimeLimit + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.toolVersion != summaries.head.toolVersion)) {
      printWarning("\t\tRuns were executed with different versions of the tool!")
      summaries.foreach(summary => printInfo("\t" + summary.fullToolName + "(" +
                                             summary.toolVersion + ")" + " on " + summary.startDate + ": " +
                                             summary.toolVersion + " (" + summary.ymlFileName + ")"))
    }
  }
}
