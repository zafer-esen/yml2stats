package yml2stats

import scala.io.Source
import net.jcazevedo.moultingyaml._
import yml2stats.Benchmarks._
import Benchmarks.MyYamlProtocol._
import yml2stats.parser._
import java.util.Date
import Settings._
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

  private def sanitizeToolNameForLatex(toolName: String): String = {
    Settings.latexToolNameReplacements.getOrElse(toolName, toolName.replace("_", "\\_"))
  }

  private def sanitizeEncodingForLatex(encoding: String): String = {
    val baseEncoding = getCleanEncodingName(encoding)
    Settings.latexEncodingReplacements.get(baseEncoding) match {
      case Some(replacement) => replacement
      case None => encoding.replace("_", "\\_")
    }
  }

  // A tuple for sorting runs: (tool name)
  private def getSortTuple(p: (Summary, RunInfos)): String = {
    p._1.toolName
  }

  private def printTable5TextFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    val sortedRuns = runs.sortBy(getSortTuple)

    val hasIncorrect = runs.exists { case (_, r) => r.incorrectRuns.nonEmpty }

    val header = if (hasIncorrect)
      Seq("Tool", "Sat (corr)", "Unsat (corr)", "Timeout", "Error", "Unknown", "Total")
    else
      Seq("Tool", "Sat", "Unsat", "Timeout", "Error", "Unknown", "Total")

    val data = sortedRuns.map { case (summary, r) =>
      val safeStr = if (hasIncorrect) s"${r.satRuns.length} (${r.correctSatRuns.length})" else s"${r.satRuns.length}"
      val unsafeStr = if (hasIncorrect) s"${r.unsatRuns.length} (${r.correctUnsatRuns.length})" else s"${r.unsatRuns.length}"
      val timeoutStr = s"${r.timeoutRuns.length}"
      val errorStr = s"${r.errorRuns.length}"
      val unknownStr = s"${r.unknownRuns.length}"
      val totalStr = s"${r.runs.length}"
      Seq(summary.toolName, safeStr, unsafeStr, timeoutStr, errorStr, unknownStr, totalStr)
    }

    val allRows = header +: data
    val colWidths = allRows.transpose.map(col => col.map(_.length).max)

    def pad(s: String, width: Int) = s.padTo(width, ' ')

    println(header.zip(colWidths).map { case (h, w) => pad(h, w) }.mkString(" | "))
    println(colWidths.map(w => "-" * w).mkString("-|-"))

    data.foreach { row =>
      println(row.zip(colWidths).map { case (cell, w) => pad(cell, w) }.mkString(" | "))
    }
  }

  private def printTable6TextFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    val orderedRuns = runs.sortBy(getSortTuple)
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
      println(f"(${(i + 1)}%2d): ${summary.toolName}")
    }
  }

  private def printTable5LatexFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    println("% For this table, please include \\usepackage{booktabs} in your LaTeX preamble.")
    val sortedRuns = runs.sortBy(getSortTuple)

    val hasIncorrect = runs.exists { case (_, r) => r.incorrectRuns.nonEmpty }

    val headerRow = if (hasIncorrect)
      "Tool & Sat (corr) & Unsat (corr) & Timeout & Error & Unknown & Total \\\\ \\midrule"
    else
      "Tool & Sat & Unsat & Timeout & Error & Unknown & Total \\\\ \\midrule"

    val dataRows = sortedRuns.map { case (summary, r) =>
      val safeTotal = r.satRuns.length
      val safeCorrect = r.correctSatRuns.length
      val unsafeTotal = r.unsatRuns.length
      val unsafeCorrect = r.correctUnsatRuns.length
      val timeout = r.timeoutRuns.length
      val error = r.errorRuns.length
      val unknown = r.unknownRuns.length
      val total = r.runs.length
      val toolName = sanitizeToolNameForLatex(summary.toolName)

      val safeStr = if (hasIncorrect) s"$safeTotal ($safeCorrect)" else s"$safeTotal"
      val unsafeStr = if (hasIncorrect) s"$unsafeTotal ($unsafeCorrect)" else s"$unsafeTotal"

      s"$toolName & $safeStr & $unsafeStr & $timeout & $error & $unknown & $total \\\\"
    }.mkString("\n")
    val latexTableString =
      s"""\\begin{table}[h]
         |  \\centering
         |  \\caption{Combined Results Summary}
         |  \\label{tbl:combined-results-summary}
         |  \\begin{tabular}{lrrrrrrr}
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
    val orderedRuns = runs.sortBy(getSortTuple)
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
    val sortedRuns = runs.sortBy(getSortTuple)

    val hasIncorrect = runs.exists { case (_, r) => r.incorrectRuns.nonEmpty }

    var header = Seq("Tool", "Sat", "Unsat")
    if (hasIncorrect) {
      header = header :+ "Incorrect"
    }
    header = header ++ Seq("Unknown", "Total")

    val data = sortedRuns.map { case (summary, r) =>
      val safeCorrect = r.correctSatRuns.length
      val unsafeCorrect = r.correctUnsatRuns.length
      val incorrect = r.incorrectRuns.length
      val unknownStr = s"${r.unknownRuns.length + r.errorRuns.length + r.timeoutRuns.length}"
      val totalStr = s"${r.runs.length}"

      var row = Seq(summary.toolName, safeCorrect.toString, unsafeCorrect.toString)
      if (hasIncorrect) {
        row = row :+ incorrect.toString
      }
      row = row ++ Seq(unknownStr, totalStr)
      row
    }

    val allRows = header +: data
    val colWidths = allRows.transpose.map(col => col.map(_.length).max)

    def pad(s: String, width: Int) = s.padTo(width, ' ')

    println(header.zip(colWidths).map { case (h, w) => pad(h, w) }.mkString(" | "))
    println(colWidths.map(w => "-" * w).mkString("-|-"))

    data.foreach { row =>
      println(row.zip(colWidths).map { case (cell, w) => pad(cell, w) }.mkString(" | "))
    }
  }

  private def printTable5SimpleLatexFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    println("% For this table, please include \\usepackage{booktabs} in your LaTeX preamble.")
    val sortedRuns = runs.sortBy(getSortTuple)
    val headerRow = "Tool & Sat & Unsat & Incorrect & Unknown & Total \\\\ \\midrule"
    val dataRows = sortedRuns.map { case (summary, r) =>
      val safeCorrect = r.correctSatRuns.length
      val unsafeCorrect = r.correctUnsatRuns.length
      val incorrect = r.incorrectRuns.length
      val unknown = r.unknownRuns.length + r.errorRuns.length + r.timeoutRuns.length
      val total = r.runs.length
      val toolName = sanitizeToolNameForLatex(summary.toolName)
      s"$toolName & $safeCorrect & $unsafeCorrect & $incorrect & $unknown & $total \\\\"
    }.mkString("\n")
    val latexTableString =
      s"""\\begin{table}[h]
         |  \\centering
         |  \\caption{Simplified Combined Results Summary}
         |  \\label{tbl:combined-results-summary-simple}
         |  \\begin{tabular}{lrrrrr}
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
    val sortedRuns = runs.sortBy(getSortTuple)
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
      println(f"(${(i + 1)}%2d): ${summary.toolName}")
    }
  }

  private def printMatrixLatexFormat(runs: Seq[(Summary, RunInfos)]): Unit = {
    println("% For this table, please include \\usepackage{booktabs} and " +
            "\\usepackage{rotating} in your LaTeX preamble.")
    val sortedRuns = runs.sortBy(getSortTuple)
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

  /**
   * Creates virtual portfolios based on definitions in Settings.
   * A portfolio's result for a benchmark is the result of the fastest
   * constituent run that solved it.
   */
  private def createVirtualPortfolios(runs: Seq[(Summary, RunInfos)]): Seq[(Summary, RunInfos)] = {
    if (Settings.virtualPortfolios.isEmpty || !Settings.doPortfolio) {
      return runs
    }

    val newPortfolios = for (portfolio <- Settings.virtualPortfolios) yield {
      printInfo(s"Creating virtual portfolio: ${portfolio.name}")

      val constituentRuns = runs.filter { case (summary, _) =>
        val toolMatch = portfolio.tools.contains("all") || portfolio.tools.contains(summary.toolName)
        val encodingMatch = portfolio.encodings.contains("all") || portfolio.encodings.contains(getCleanEncodingName(summary.notes))
        toolMatch && encodingMatch
      }

      if (constituentRuns.isEmpty) {
        printWarning(s"Warning: No runs found for virtual portfolio '${portfolio.name}'. Skipping.")
        None
      } else {
        val allBenchmarkNames = constituentRuns.flatMap(_._2.runs.map(_.bmBaseName)).distinct

        val portfolioRunInfos = for (bmName <- allBenchmarkNames.toList) yield {
          val candidateRuns = constituentRuns.flatMap(_._2.getRun(bmName))

          val solvedRuns = candidateRuns.filter(r => r.result == True || r.result == False)

          val bestRun = if (solvedRuns.nonEmpty) {
            solvedRuns.minBy(_.duration)
          } else {
            // Fallback logic
            candidateRuns.find(_.result == Timeout)
                         .orElse(candidateRuns.find(_.result == Unknown))
                         .orElse(candidateRuns.headOption) // Should always exist
                         .get
          }
          bestRun
        }

        val representativeSummary = constituentRuns.head._1
        val portfolioSummary = representativeSummary.copy(
          toolName = "V. Portfolio",
          notes = portfolio.name,
          ymlFileName = "virtual.yml"
          )

        Some((portfolioSummary, RunInfos(portfolioRunInfos)))
      }
    }
    runs ++ newPortfolios.flatten
  }

  def main(args: Array[String]) : Unit = {
    val usage =
      """|Usage: yml2stats [options] inFileName | inDirName
         |
         |Processes .yml files from benchmark runs and generates summary tables.
         |
         |inFileName      : A single .yml input file to process.
         |inDirName       : A directory containing .yml files to process.
         |
         |Options:
         |  -summary      : Print simplified summary table in text format.
         |  -summary-tex  : Print simplified summary table in LaTeX format.
         |  -summary-ext  : Print extended summary table in text format (was -table5tex default without tex).
         |  -summary-ext-tex : Print extended summary table in LaTeX format.
         |  -details      : Print detailed per-benchmark results in text format.
         |  -details-tex  : Print detailed per-benchmark results in LaTeX format.
         |  -matrix       : Print matrix of comparative results in text format.
         |  -matrixtex    : Print matrix of comparative results in LaTeX format.
         |  -cactus-pdf   : Generate a cactus plot in PDF format.
         |  -cactus-plotly: Generate a cactus plot using Plotly (DISABLED).
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
        case "-matrix" :: tail =>
          doMatrixText = true
          remainingArgs = tail
        case "-matrixtex" :: tail =>
          doMatrixTex = true
          remainingArgs = tail
        case "-cactus-plotly" :: tail =>
          doCactusPlotly = true
          remainingArgs = tail
        case "-cactus-pdf" :: tail =>
          doCactusPdf = true
          remainingArgs = tail
        case opt :: tail if opt.startsWith("-") =>
          println(s"Unknown option: $opt\n")
          println(usage)
          return
        case path :: tail =>
          if (inFileName.nonEmpty) {
            println("Error: More than one input file/directory specified.\n")
            println(usage)
            return
          }
          inFileName = path
          remainingArgs = tail
      }
    }

    if (!doTable5Tex && !doTable6Tex && !doTable6Text && !doTable5SimpleText && !doTable5SimpleTex && !doMatrixText && !doMatrixTex && !doCactusPdf && !doCactusPlotly) {
      doTable5Text = true
    }

    if (inFileName.isEmpty) {
      println("An input filename must be provided.\n")
      println(usage)
      return
    }

    val in = new java.io.File(inFileName)
    if (!in.exists) {
      println(inFileName + " not found!"); return
    }

    val files = if (in.isDirectory) {
      printInfo("Processing all .yml files under " + in + "...")
      val fileList = in.listFiles()
      if (fileList != null) fileList.toList else List.empty
    } else {
      List(in)
    }

    val yamlAsts = for (file <- files if file.getName.endsWith(".yml")) yield {

      val inFile = Source.fromFile(file)
      val source = inFile.getLines.mkString("\n")

      inFile.close

      try {
        (file.getName, source.parseYaml)
      } catch {
        case _ : Throwable =>
          throw new Exception("Could not parse " + file.getName)
      }

    }

    if (yamlAsts.isEmpty) {
      println("No .yml files found in " + inFileName)
      return
    }

////////////////////////////////////////////////////////////////////////////////
// Convert YAML ASTs into useful data structures

    val unmergedToolRuns : Seq[(Summary, Seq[RunInfo])] =
      for ((fileName, ast) <- yamlAsts) yield {
        printInfo("Processing " + fileName + "...")
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

        val runInfos = RunInfos(for (rawRunInfo <- rawRunInfos) yield {
          val result = outputParser(rawRunInfo.toolOutput, rawRunInfo.bmName)
          val expected = expectedStatusParser(rawRunInfo.expected)
          RunInfo(rawRunInfo.bmName, expected, result,
                  rawRunInfo.duration.dropRight(1).toDouble) // todo properly parse duration
        })
        printInfo("done! " + runInfos.length + " runs found.")
        (Summary(rawSummary, fileName), runInfos.runs)
      }

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

    val toolRuns = createVirtualPortfolios(toolRunsWithoutVP.toSeq)

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
    val commonBenchmarkNames =
      (for (run <- smallestRuns._2.runs
            if toolRuns.forall(p => p._2.runs.exists(run2 =>
                                                       run2.bmBaseName == run.bmBaseName )))
      yield run.bmBaseName).toSet

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
    if (doMatrixTex) {
      printSep()
      printMatrixLatexFormat(filteredToolRuns)
    }

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

    {
      def runsAreConsistent (run1 : RunInfo, run2 : RunInfo) = {
        run1.result match {
          case True if run2.result == False => false
          case False if run2.result == True => false
          case _ => true
        }
      }
      var inconsistentCount = 0
      println
      for (bmName <- finalCommonBenchmarkNames) {
        val runPerTool =
          filteredToolRuns.map{case (summary, toolRuns) =>
            (summary.fullToolName ,toolRuns.getRun(bmName).get)}
        for (Seq((tool1, run1), (tool2, run2)) <- runPerTool.combinations(2)) {
          if(!runsAreConsistent(run1, run2)) {
            inconsistentCount += 1
            printInfo(run1.bmBaseName + " does not have consistent results in" +
                      " all tools:\n\t" +
                      runPerTool.map{
                        case (tool, run) => tool + " (expected: " + run.expected +
                                            ", result: " + run.result + ")"
                      }.mkString("\n\t"))
          }
        }
      }
      if(inconsistentCount > 0)
        printWarning("Warning: detected " + inconsistentCount + " inconsistent runs!")
      else
        printInfo("No inconsistent runs detected!")
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
