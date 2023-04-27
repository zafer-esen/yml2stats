package yml2stats

import scala.collection.mutable.{ArrayBuffer, HashSet => MHashSet}
import scala.io.Source
import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors._
import scala.collection.JavaConverters._
import net.jcazevedo.moultingyaml._
import yml2stats.Benchmarks._
import Benchmarks.MyYamlProtocol._
import yml2stats.parser.{CPAOutputParser, EldaricaOutputParser, MonoceraExpectedStatusParser, MonoceraOutputParser, SMTExpectedStatusParser, SVExpectedStatusParser, SVOutputParser, SeaHornOutputParser, ToolOutputParser, Z3OutputParser}



import java.util.{Date, stream}
import Settings._

import scala.collection.mutable
import scala.language.postfixOps

object Main extends App {

  def printWarning(s: String) = {
    if (verbosityLevel >= 1)
      println(s)
  }
  def printInfo(s: String = "") = {
    if (verbosityLevel >= 2)
      println(s)
  }
  def printMoreInfo(s: String) = {
    if (verbosityLevel >= 3)
      println(s)
  }

  override def main(args: Array[String]): Unit = {
    val usage =
      """Usage: yml2stats [-appendix] inFileName | inDirName
  inFileName      : input file to process
  inDirName       : input directory to process
                    (only files with .yml extension are considered)
  -appendix       : print the long table in the appendix

e.g., "yml2stats /path/to/dir" will collect all .yml files in dir and produce
      an output.
"""

    if (args.length == 0) {
      println(usage);
      return
    }
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    def parseOptions(list: List[String]): Unit = {
      list match {
        case Nil => // nothing
        case string :: Nil =>
          inFileName = string
          parseOptions(list.tail)
        case option :: _ if option == "-appendix" =>
          printAppendixTable = true
          parseOptions(list.tail)
        case option :: _ =>
          println("Unknown option: " + option + "\n")
      }
    }

    parseOptions(arglist)

    if (inFileName.isEmpty) {
      println("An input filename must be provided.\n")
      println(usage)
      return
    }

    val in = new java.io.File(inFileName)
    if (!in.exists) {
      println(inFileName + " not found!");
      return
    }

    def walkFiles(file : File) : Array[File] = {
      val curFiles = file.listFiles
      curFiles ++ curFiles.filter(_.isDirectory).flatMap(walkFiles)
    }

    val files : List[File] = if (in.isDirectory) {
      printInfo("Processing all .yml files under " + in + "...")
      walkFiles(in).toList
    } else in.listFiles(_.isFile).toList

////////////////////////////////////////////////////////////////////////////////
// Parse input files and create YAML ASTs

    val yamlAsts = for (file <- files if file.getName.endsWith(".yml")) yield {
      printInfo("Processing " + file + "...")

      val inFile = Source.fromFile(file)
      val source = inFile.getLines.mkString("\n")

      inFile.close

      try {
        (file, source.parseYaml)
      } catch {
        case _: Throwable =>
          throw new Exception("Could not parse " + file)
      }

    }

    if (yamlAsts.isEmpty) {
      println("No .yml files found in " + inFileName)
      return
    }

////////////////////////////////////////////////////////////////////////////////
// Convert YAML ASTs into useful data structures

    val unmergedToolRuns: Seq[(Summary, RunInfos)] =
      (for ((fileName, ast) <- yamlAsts) yield {
        printInfo("Processing " + fileName + "...")
        val (rawSummary, rawRunInfos) =
          ast.convertTo[(SummaryRaw, Seq[RunInfoRaw])]

        printInfo("Parsing tool outputs...")
        val outputParser: ToolOutputParser =
          rawSummary.toolName match { // todo: maybe another method?
            case s if s.toLowerCase contains "eld" =>
              EldaricaOutputParser
            case s if s.toLowerCase contains "z3" => Z3OutputParser
            case _ if rawRunInfos.head.bmName.endsWith("yml") =>
              SVOutputParser
            case s if s.toLowerCase contains "cpa"      => CPAOutputParser
            case s if s.toLowerCase contains "monocera" => MonoceraOutputParser
            case s if s.toLowerCase contains "seahorn"  => SeaHornOutputParser
            case s if s.toLowerCase contains "tricera"  => MonoceraOutputParser
            case s =>
              throw new Exception(
                "An output parser for the tool " +
                  s + " is not yet implemented.")
          }

        printInfo("Parsing expected status fields...")
        val expectedStatusParser =
          rawSummary.toolName match { // todo: maybe another method?
            case s
                if (s.toLowerCase contains "eld") ||
                  (s.toLowerCase contains "z3") ||
                  (s.toLowerCase contains "cpa") => // todo: cpa only if
              // it is printed in the same way as SMT expected status
              SMTExpectedStatusParser
            case s if (s.toLowerCase contains "monocera") =>
              MonoceraExpectedStatusParser
            case s if (s.toLowerCase contains "tricera") =>
              MonoceraExpectedStatusParser
            case s if (s.toLowerCase contains "seahorn") =>
              MonoceraExpectedStatusParser
            case _ =>
              SVExpectedStatusParser
          }

        val runInfos = RunInfos(for (rawRunInfo <- rawRunInfos) yield {
          val result   = outputParser(rawRunInfo.toolOutput, rawRunInfo.bmName)
          val expected = expectedStatusParser(rawRunInfo.expected)
          val categoryName: String = if (categorizeBasedOnDirectory) {
            // filename is something like .../.../.../directory/benchmark.ext
            // try to use directory as category name
            val splitName   = rawRunInfo.bmName.split("/")
            val categoryInd = splitName.length - 2
            if (categoryInd < splitName.length && categoryInd >= 0)
              splitName(categoryInd)
            else ""
          } else ""
          val extraStats =
            if (rawRunInfo.toolOutput.exists(line =>
                  line contains "space sizes"))
              MonoceraOutputParser.getExtraStats(rawRunInfo.toolOutput)
            else None
          RunInfo(rawRunInfo.bmName,
                  expected,
                  result,
                  rawRunInfo.duration.dropRight(1).toDouble,
                  categoryName,
                  extraStats)
          // todo properly parse duration
        })
        printInfo("done! " + runInfos.length + " runs found.")

        if (categorizeBasedOnDirectory) {
          printInfo("\nCategorizing files based on directory...")
          val categorizedRuns =
            for ((category, runs) <- runInfos.runs.groupBy(_.category))
              yield (Summary(rawSummary, fileName.getName, category), RunInfos(runs))
          printInfo(fileName.getName)
          for ((summary, runs) <- categorizedRuns) {
            printInfo(summary.category + ": " + runs.length + " runs.")
          }
          categorizedRuns
        } else {
          Seq((Summary(rawSummary, fileName.getName), runInfos))
        }
      }).flatten

    if (mergeYmlFiles && combineResults)
      printWarning(
        "Cannot enable both mergeYmlFiles and combineResults! " +
          "Defaulting to merging.")

    def combineRuns(runs: Seq[(RunInfo, Date)]): (RunInfo, Date) = {
      //   - if a benchmark is UNSAT   in any of the combined results, result
      //   will be UNSAT
      //else if a benchmark is TIMEOUT in any of the combined results, result
      // will be TIMEOUT
      //else if a benchmark is SAT     in all of the combined results, result
      // will be SAT
      //else if a benchmark is ERROR   in any of the combined results, result
      // will be ERROR
      //else if a benchmark is UNSAT   in any of the combined results, result
      // will be UNSAT
      //else if a benchmark is SAT/UNKNOWN in all of the com. results, result
      // will be UNKNOWN
//      runs.foreach(run => println(run._1.result)); println
      runs.find(run => run._1.result == False) match {
        case Some(run) => run
        case _ =>
          runs.find(run => run._1.result == Timeout) match {
            case Some(run) => run
            case _ if runs.forall(run => run._1.result == True) =>
              runs.head
            case _
                if runs.forall(
                  run => run._1.result == Unknown || run._1.result == True) =>
              runs.find(run => run._1.result == Unknown).get
            case _ if runs.forall(run => run._1.result.isInstanceOf[Error]) =>
              runs.find(run => run._1.result.isInstanceOf[Error]).get
            case _
                if runs.forall(run =>
                  run._1.result == Unknown || run._1.result
                    .isInstanceOf[Error]) =>
              runs.find(run => run._1.result == Unknown).get
            case _
                if runs.forall(run =>
                  run._1.result == True || run._1.result
                    .isInstanceOf[Error]) =>
              runs.find(run => run._1.result == True).get
            case _
                if runs.forall(run =>
                  run._1.result == False || run._1.result
                    .isInstanceOf[Error]) =>
              runs.find(run => run._1.result == False).get
            case _ =>
              throw new Exception(
                "Cannot combine runs!" + runs.map(run => run._1.result))
          }
      }
    }

    val allToolRuns: Iterable[(Summary, RunInfos)]
      with PartialFunction[Summary with Int, Object] =
      if (mergeYmlFiles || combineResults) {
        println
        printWarning("Merging files with same tool name and options...")
        val groupedToolRuns =
          if (ignoreDifferentOptions) {
            unmergedToolRuns.groupBy(
              p =>
                p._1.category + p._1.toolName + (if (ignoreDifferentNotes) ""
                                                 else p._1.notes))
          } else {
            unmergedToolRuns.groupBy(
              p =>
                if (ignoreDifferentOptionsForTools
                      contains p._1.toolName)
                  p._1.category + p._1.toolName + (if (ignoreDifferentNotes) ""
                                                   else p._1.notes)
                else
                  p._1.category + p._1.toolName + " (" +
                    p._1.toolOptions + (if (ignoreDifferentNotes) ""
                                        else p._1.notes) + ")")
          }
        for ((nameAndOpts, toBeMergedRuns) <- groupedToolRuns) yield {
          // checks to ensure merged files do not differ in any parameters
          if (toBeMergedRuns.length > 1) {
            printWarning(
              "\n\tFound " + toBeMergedRuns.length + " file(s) for "
                + nameAndOpts)
            val summaries = toBeMergedRuns.map(_._1)
            checkIfSameParameters(summaries)
            val summary =
              toBeMergedRuns.head._1 // take the summary of the first one
            val allRunsWithDate =
              toBeMergedRuns.flatMap(
                p =>
                  p._2.runs zip
                    p._2.runs.indices.map(_ => p._1.startDate))
            val runsGroupedByBmName: Seq[(String, Seq[(RunInfo, Date)])] =
              allRunsWithDate.groupBy(runs => runs._1.bmBaseName).toSeq
            val uniqueRuns: Seq[RunInfo] =
              for ((name, runsWithDate) <- runsGroupedByBmName) yield {
                if (runsWithDate.length > 1) {
                  val resultRun: (RunInfo, Date) =
                    if (mergeYmlFiles)
                      runsWithDate.maxBy(p => p._2)
                    else
                      combineRuns(runsWithDate)
                  printMoreInfo(
                    "\tFound " + runsWithDate.length + " benchmarks" +
                      " with the same" +
                      " name (" + name + ") while merging. \n" +
                      "\t\tTaking the one executed on " + resultRun._2 + ".")
                  resultRun._1
                } else runsWithDate.head._1
              }

            val runs = RunInfos(uniqueRuns)

            printWarning(
              "\tMerged " + nameAndOpts + ". New total: " + runs.length + " benchmarks.")
            (summary, runs)
          } else toBeMergedRuns.head
        }
      } else unmergedToolRuns

    val latexTables          = new mutable.HashMap[String, String]
    val combinatorialResults = new mutable.HashMap[String, ArrayBuffer[String]]

    var initializedAppendixTable = false
    var appendixTableLines : String = ""

    for ((category, toolRuns) <- allToolRuns.groupBy(_._1.category)) {

      combinatorialResults += ((category, new ArrayBuffer[String]))

//      println(category)
////////////////////////////////////////////////////////////////////////////////
// Fairness checks
      if (printFairnessWarnings) {
        println
        val summaries = toolRuns.map(_._1)
        if (summaries.exists(s => s.cpuCount != summaries.head.cpuCount)) {
          printWarning(
            "Runs were executed on systems with different CPU " +
              "counts!")
          summaries.foreach(
            summary =>
              printInfo(
                "\t" + summary.toolName +
                  ": " +
                  summary.cpuCount + " (" +
                  summary.ymlFileName + ")"))
        }
        if (summaries.exists(
              s => s.architecture != summaries.head.architecture)) {
          printWarning(
            "Runs were executed on systems with different " +
              "architectures!")
          summaries.foreach(
            summary =>
              printInfo(
                "\t" + summary.toolName +
                  ": " +
                  summary.architecture + " ("
                  + summary.ymlFileName + ")"))
        }
        if (summaries.exists(s => s.cpuModel != summaries.head.cpuModel)) {
          printWarning(
            "Runs were executed on systems with different cpu " +
              "models!")
          summaries.foreach(
            summary =>
              printInfo(
                "\t" + summary.toolName +
                  ": " +
                  summary.cpuModel + " (" +
                  summary.ymlFileName + ")"))
        }
        if (summaries.exists(s => s.memTotal != summaries.head.memTotal)) {
          printWarning(
            "Runs were executed on systems with different total " +
              "memories!")
          summaries.foreach(
            summary =>
              printInfo(
                "\t" + summary.toolName +
                  ": " +
                  summary.memTotal + " (" +
                  summary.ymlFileName + ")"))
        }
        if (summaries.exists(
              s => s.wallTimeLimit != summaries.head.wallTimeLimit)) {
          printWarning(
            "Runs were executed on systems with different wall " +
              "time limits!")
          summaries.foreach(
            summary =>
              printInfo(
                "\t" + summary.toolName +
                  ": " +
                  summary.wallTimeLimit + " ("
                  + summary.ymlFileName + ")"))
        }
        if (summaries.exists(
              s => s.cpuTimeLimit != summaries.head.cpuTimeLimit)) {
          printWarning(
            "Runs were executed on systems with different CPU time" +
              " limits!")
          summaries.foreach(
            summary =>
              printInfo(
                "\t" + summary.toolName +
                  ": " +
                  summary.cpuTimeLimit + " ("
                  + summary.ymlFileName + ")"))
        }
      }

////////////////////////////////////////////////////////////////////////////////
// Printing of individual (for each provided file) statistics

      if (printIndividualStats) {
        for ((summary, runs) <- toolRuns) {
          println(summary)
          println(runs)
          println
        }
      }

////////////////////////////////////////////////////////////////////////////////
// Printing of combined (all provided files) statistics

      // eliminate benchmarks that do not appear in one of the files

      val smallestRuns = toolRuns.minBy(pair => pair._2.length)

      // collect benchmark names that was executed by all tools
      val commonBenchmarkNames =
        (for (run <- smallestRuns._2.runs
              if toolRuns.forall(p =>
                p._2.runs.exists(run2 =>
                  run2.bmBaseName ==
                    run.bmBaseName)))
          yield run.bmBaseName).toSet

      printInfo(
        commonBenchmarkNames.size + " benchmarks were executed by all " +
          "tools.")

      val errorRunNamesForEachTool: Seq[Seq[String]] =
        toolRuns
          .map(
            p =>
              p._2.errorRuns
                .filter(
                  run =>
                    if (excludeSolverErrors) //
                      // leave only errors
                      // without solve errors
                      !run.result
                        .asInstanceOf[Error]
                        .errorTypes
                        .contains(ErrorType.Solve)
                    else
                    true // do not exclude
                  // anything
                )
                .map(_.bmBaseName))
          .toSeq
      val combinedErrorRuns: Set[String] =
        errorRunNamesForEachTool.reduce(_ union _).toSet
      val commonBenchmarkNamesWithoutErrors: Set[String] =
        commonBenchmarkNames diff combinedErrorRuns

      //toolRuns.toSeq(6)._2.runs.map(_.bmBaseName).toSet diff
      // commonBenchmarkNames

      printInfo()
      printInfo(
        commonBenchmarkNamesWithoutErrors.size +
          " benchmarks had no errors in any of the tools.")

      val commonBenchmarkNamesMaybeWithoutErrors =
        if (excludeErrors) commonBenchmarkNamesWithoutErrors
        else commonBenchmarkNames

      printInfo()
      printInfo(
        (if (excludeErrors) "Excluding" else "Including") +
          " benchmarks that any tool reported an error for in comparisons.\n"
      )
      // todo: do not exclude specific types of errors? (e.g., solve)
      //  alternatively categorize these as "unknown"

      val incorrectRunNamesForEachTool: Seq[Seq[String]] =
        toolRuns.map(p => p._2.incorrectRuns.map(_.bmBaseName)).toSeq
      val combinedIncorrectRuns: Set[String] =
        incorrectRunNamesForEachTool.reduce(_ union _).toSet
      val commonBenchmarkNamesWithoutIncorrect: Set[String] =
        commonBenchmarkNamesMaybeWithoutErrors diff combinedIncorrectRuns

      printInfo()
      printInfo(
        commonBenchmarkNamesWithoutIncorrect.size +
          " benchmarks had no incorrect results in any of the tools.")
      printInfo()
      printInfo(
        (if (excludeIncorrect) "Excluding" else "Including") +
          " benchmarks that any tool returned an incorrect result for in " +
          "comparisons.\n"
      )

      val finalCommonBenchmarkNames =
        if (excludeIncorrect) commonBenchmarkNamesWithoutIncorrect
        else commonBenchmarkNamesMaybeWithoutErrors

      //commonBenchmarkNamesWithoutErrors.foreach(println)

      val filteredToolRuns: Seq[(Summary, RunInfos)] =
        (for ((summary, runs) <- toolRuns) yield {
          val filteredRuns = runs.runs.filter(
            run =>
              finalCommonBenchmarkNames
                contains run.bmBaseName)
          (summary, RunInfos(filteredRuns))
        }).toSeq

      val portfolioRuns: Seq[RunInfo] = if (portfolioToolNames.nonEmpty) {
        val runsInPortfolio =
          filteredToolRuns.filter {
            case (summary, _) => portfolioToolNames contains summary.toolName
          }
        val runsGroupedByBmName: Map[String, Seq[RunInfo]] =
          runsInPortfolio.flatMap(p => p._2.runs).groupBy(run => run.bmBaseName)

        // for each run in portfolio, take the first result that is sat/unsat if
        // others did not report a conflicting result. Set conflicting results
        // to unknown?
        // todo: take first returned result instead?
        (for ((_, runPerTool) <- runsGroupedByBmName) yield {
          var winningRun: RunInfo = runPerTool.minBy(run => run.duration)
          for (run <- runPerTool) {
            run.result match {
              case True if winningRun.result != False =>
                winningRun = run
              case False if winningRun.result != True =>
                winningRun = run
              case c
                  if winningRun.result != True && winningRun.result !=
                    False =>
                winningRun = run
              case c
                  if c == True && winningRun.result == False ||
                    c == False && winningRun.result == True => // conflicting
                // results
                winningRun = RunInfo(
                  run.bmName,
                  run.expected,
                  Unknown,
                  runPerTool.maxBy(run => run.duration).duration,
                  run.category)
              case _ => // ignore
            }
          }
          winningRun
        }).toSeq
      } else Nil

      // todo print relevant parts of the summaries of each tool (timeouts etc.)

      if (printCombinedResults) {
        val offset = toolRuns
          .map(runs => runs._1.toolName + runs._1.category)
          .maxBy(_.length)
          .length
        val tabSpaces = 4
        val columnLabels = Seq("sat\t\t\t",
                               "unsat\t\t",
                               "unknown\t\t",
//                               "timeout\t\t",
                               "error\t\t",
//                               "sat+unsat(corr.)\t",
//                               "unsound\t\t",
//                               "incomplete\t",
//                               "incorrect")
                               )
        val firstTabCount = (offset.toDouble / tabSpaces).ceil.toInt + 1
        //val firstTabCount = (minTabCount + (offset.toDouble / tabSpaces)
        // .floor.toInt) + 1
        println("=" * 80)
        println(category)
        println("-" * 80)
        print("\t" * firstTabCount)
        println(columnLabels.mkString(""))
        for ((summary, runs) <- filteredToolRuns) {
          val firstColText = summary.toolName
          val tabsAfterToolName = firstTabCount - (firstColText.length.toDouble /
            tabSpaces).floor.toInt
          print(firstColText + "\t" * tabsAfterToolName) // todo: print
          // anything else? notes? version?
          val columns = Seq(
            runs.satRuns
              .diff(runs.unsoundRuns)
              .length,
            runs.unsatRuns
              .diff(runs.incompleteRuns)
              .length,
            runs.unknownRuns.length + runs.timeoutRuns.length,
//            runs.timeoutRuns.length,
            runs.errorRuns.length + runs.incorrectRuns.length,
//            (runs.satRuns.length + runs.unsatRuns.length) + "" +
//              "(" + runs.correctRuns.length + ")",
//            runs.unsoundRuns.length,
//            runs.incompleteRuns.length,
//            runs.incorrectRuns.length
          )
          println(columns.mkString("\t\t\t"))
        }
        if (portfolioRuns.nonEmpty) {
          val toolName = "portfolio"
          val runs     = RunInfos(portfolioRuns)
          val tabsAfterToolName = firstTabCount - (toolName.length.toDouble /
            tabSpaces).floor.toInt
          print(toolName + "\t" * tabsAfterToolName) // todo: print anything
          // else? notes? version?
          val columns = Seq(
            runs.satRuns.length + "(" + runs.satRuns
              .diff(runs.unsoundRuns)
              .length + ")",
            runs.unsatRuns.length + "(" + runs.unsatRuns
              .diff(runs.incompleteRuns)
              .length + ")",
            runs.unknownRuns.length,
            runs.timeoutRuns.length,
            runs.errorRuns.length,
            (runs.satRuns.length + runs.unsatRuns.length) + "" +
              "(" + runs.correctRuns.length + ")",
            runs.unsoundRuns.length,
            runs.incompleteRuns.length,
            runs.incorrectRuns.length
          )
          println(columns.mkString("\t\t\t"))
        }
        println("=" * 80)
        val sameNamedRuns = filteredToolRuns.groupBy(_._1.toolName)
        for ((toolName, runs) <- sameNamedRuns if runs.length > 1) {
          println("(in order above) " + toolName +" refers to:")
          println(runs.map(r => "  " +
                                r._1.toolName + " (" +
                                r._1.toolOptions + " started on " +
                                r._1.startDate +")")
                      .mkString("\n") + "\n")
        }
      }

      if (printCombinedResultsLatex) {
//        println("\nLaTeX table (total count: " + commonBenchmarkNames.size +
//                ") for " + category +
//                "\n\n")
        val headerRow = Seq("&solved&",
                            "total solved" +
                              " \\\\\\midrule").mkString("")

        val dataRows =
          (for ((summary, runs) <- filteredToolRuns.sortBy(runs =>
                  runs._2.satRuns.length + runs._2.unsatRuns.length)) yield {
            val firstColumn = summary.toolName + "&" // todo: print anything else?
            // notes? version?
            val columns = Seq(
              runs.correctRuns.length,
              runs.length + "\\\\"
            )
            firstColumn + columns.mkString("&")
          }).mkString("\n")

        val portfolioRow: String =
          if (portfolioRuns.nonEmpty) {
            val firstColumn = "\\emph{portfolio}&"
            val runs        = RunInfos(portfolioRuns)
            val columns = Seq(
              runs.satRuns.length,
              runs.unsatRuns.length,
              commonBenchmarkNames.size - (runs.satRuns.length + runs.unsatRuns.length),
              (runs.satRuns.length + runs.unsatRuns.length) + "\\\\\\midrule"
            )
            firstColumn + columns.mkString("&")
          } else ""

        val latexTableString =
          s"""\\begin{table}
             |  \\begin{tabular}{lrr}
             |     $headerRow
             |     $dataRows
             |     $portfolioRow
             |   \\end{tabular}
             |   \\caption{Results for ${Util.sanitizeString(category)}
             |   \\label{tbl:${Util.sanitizeString(category)}-results}
             | \\end{table}
            """.stripMargin

        latexTables += ((category, latexTableString))
      }

////////////////////////////////////////////////////////////////////////////////
// Consistency checks
      {
        def runsAreConsistent(runPerTool: Seq[(String, RunInfo)]): Boolean = {
          !(runPerTool.exists(run => run._2.result == True) &&
            runPerTool.exists(run => run._2.result == False))
        }
        var inconsistentCount = 0
        println
        for (bmName <- finalCommonBenchmarkNames) {
          val runPerTool =
            filteredToolRuns.map {
              case (summary, toolRuns) =>
                (summary.toolName, toolRuns.getRun(bmName).get)
            }
          if (!runsAreConsistent(runPerTool)) {
            printInfo(
              runPerTool.head._2.bmBaseName + " does not have " +
                "consistent results in" +
                " all tools:\n\t" +
                runPerTool
                  .map {
                    case (tool, run) =>
                      tool + " (expected: " + run.expected +
                        ", result: " + run.result + ")"
                  }
                  .mkString("\n\t"))
            printWarning("inconsistent run name: " + bmName)
            inconsistentCount += 1
          }
        }
        if (inconsistentCount > 0)
          printWarning(
            "Warning: detected " + inconsistentCount + " " +
              "inconsistent runs!")
        else
          printInfo("No inconsistent runs detected!")
      }
////////////////////////////////////////////////////////////////////////////////

// Print combinatorial results (i.e., correct results that a tool had an
// answer for but a subset of others did not )
      if (printCombinatorialResults) {
        println
        for (((summary, runs), i) <- filteredToolRuns.zipWithIndex) {
          // runs that *only* this tool solved
          var uniqueDiffRuns = runs
          val comparativeText: String =
            (for (j <- filteredToolRuns.indices if i != j) yield {
              uniqueDiffRuns = uniqueDiffRuns - filteredToolRuns(j)._2
              val diffRuns = runs - filteredToolRuns(j)._2 // runs that this
              // tool solved that some other tool could not solve
              summary.toolName + " solved " + diffRuns.satRuns.length +
                "/" +
                diffRuns.unsatRuns.length + " that " +
                filteredToolRuns(j)._1.toolName + " could not solve."
            }).mkString("\\\\\n")

          val overallText: String =
            summary.toolName + " solved " + uniqueDiffRuns.satRuns.length + "/" +
              uniqueDiffRuns.unsatRuns.length + " that any other tool " +
              "could not solve."

          combinatorialResults(category) += comparativeText + "\\\\\n " + overallText
        }
      }

      // todo: wip, only here for testing purposes
      if (!disableAllPlots && (plotDurations || plotDurationsFile) &&
          filteredToolRuns.length > 1) {
        println
        println("Generating durations plots")
        //if (Settings.produceSinglePlotForAllCategories) {
        for (Seq(toolRuns1, toolRuns2) <- filteredToolRuns.combinations(2)) {
          Plotting.plotDuratıons(toolRuns1, toolRuns2, Some(category))
        }
//        } else {
//          val categorizedAllRuns = filteredToolRuns.groupBy(_._1.category)
//          for ((category, runs) <- categorizedAllRuns;
//            Seq(toolRuns1, toolRuns2) <- runs.combinations(2)) {
//              Plotting.plotDuratıons(toolRuns1, toolRuns2, Some(category + " - "))
//            }
//        }
      }

      if (!disableAllPlots && (plotCactus || plotCactusFile)) {
        println
        println("Generating cactus plot")
        Plotting.plotCactus(filteredToolRuns ++
                              Seq(
                                (Summary("Portfolio",
                                         "",
                                         "",
                                         -1,
                                         -1,
                                         "",
                                         "",
                                         0,
                                         "",
                                         "",
                                         null,
                                         "",
                                         "",
                                         "",
                                         ""),
                                 RunInfos(portfolioRuns))),
                            Some(category))
      }

      // Print a list of error benchmarks if printErrorsForEachTool is set
      if (printComparedRunNames) {
        println(
          "Compared run names (basename) (" +
            filteredToolRuns.head._2.runs.length + " runs)\n" + "-" * 80)
        filteredToolRuns.head._2.runs.foreach(run => println(run.bmBaseName))
        println("=" * 80)
      }

      // Print a list of error benchmarks if printErrorsForEachTool is set
      if (printErrorsForEachTool) {
        println("Error runs\n" + "-" * 80)
        filteredToolRuns.foreach {
          case (summary, runs) =>
            println(summary.toolName)
            runs.errorRuns.foreach(run =>
              println("  " + run.bmName + ": " + run.result + ")"))
        }
        println("=" * 80)
      }
      // Print a list of unsound benchmarks if printErrorsForEachTool is set
      if (printUnsoundForEachTool) {
        println("Unsound runs\n" + "-" * 80)
        filteredToolRuns.foreach {
          case (summary, runs) =>
            println(summary.toolName)
            runs.unsoundRuns.foreach(run =>
              println("  " + run.bmName + ": " + run.result + ")"))
        }
        println("=" * 80)
      }
      // Print a list of incomplete benchmarks if printErrorsForEachTool is set
      if (printIncompleteForEachTool) {
        println("Incomplete runs\n" + "-" * 80)
        filteredToolRuns.foreach {
          case (summary, runs) =>
            println(summary.toolName)
            runs.incompleteRuns.foreach(run =>
              println("  " + run.bmName + ": " + run.result + ")"))
        }
        println("=" * 80)
      }

      if (printPerFilenameStats) {
        val summaries = filteredToolRuns.map(_._1)
        val results   = filteredToolRuns.map(_._2)

        val numTools = summaries.size
        val timeout  = summaries.head.wallTimeLimit

        val runs = results.map(_.runs.sortBy(_.bmBaseName))

        val svPaths =
          Util.readRootDirectoriesFromFile("./og_filepaths.txt")
        // print tool names in header row
        def latexName(name: String): String = {
          name match {
            case s if s.toLowerCase == "tricera"    => "\\tricera"
            case s if s.toLowerCase == "cpachecker" => "\\cpachecker"
            case s if s.toLowerCase == "monocera"   => "\\monocera"
            case s if s.toLowerCase == "seahorn"    => "\\seahorn"
            case _                                  => name
          }
        }

        val headerRow = " & " + summaries
          .map(s => latexName(s.toolName))
          .mkString(" & ") + "\\\\"

        // iterate over all results to create a detailed table
        val dataRows = (for (j <- 0 until results.head.length)
          yield { // iterate over each benchmark
            // print benchmark name in first column
            val baseName = runs.head(j).bmBaseName
            val fullName = svPaths get baseName match {
              case Some(fullPath) => fullPath
              case None           => baseName
            }
            Util.sanitizeString(fullName) + " & " +
              (for (i <- 0 until numTools) yield { // iterate over each tool
                // then for each tool print result + duration
                val run = runs(i)(j)
                //val duration = if (run.duration > 300) 300 else run.duration
                val res = run.result match { // conver
                  case e: Error => "Error"
                  case True    => "\\textbf{\\textcolor{green!50!black}{True}}"
                  case False   => "Error" // \textcolor{red!90!black}{
                  case Unknown => "Unknown"
                  case Timeout => "T/O"
                }
                val isTimeout = run.duration >= timeout
                val duration =
                  if (isTimeout)
                    "T/O"
                  else if (run.duration >= 100)
                    f"${run.duration}%.0f"
                  else
                    f"${run.duration}%.0f"

                assert(run.bmBaseName == runs(0)(j).bmBaseName)
                if (isTimeout) duration else res + " (" + duration + ")"
              }).mkString(" & ")
          }).mkString(" \\\\\\midrule\n\t\t")

        val tableFormat = "l" + "r" * numTools

        if(printAppendixTable ) {
          if (!initializedAppendixTable) {
            initializedAppendixTable = true
            appendixTableLines += s"""% \\begin{table}
             |  \\begin{longtable}{$tableFormat}
             |  \\caption{Per benchmark results for all tools. Timeout (T/O)
             |  is $timeout s.}
             |   \\label{tbl:per-benchmark-results}\\\\
             |     $headerRow\\toprule
             | \\endfirsthead
             |     $headerRow\\toprule
             |      \\endhead
             |     """.stripMargin
          }
          appendixTableLines += s"$dataRows\\\\\\bottomrule\n"
        }

      }

      for ((summary, allRuns) <- filteredToolRuns) {
        val totalLength = allRuns.length
        val runs        = allRuns.satRuns
        val n           = runs.length
        val durations   = runs.map(_.duration)
        if (durations.nonEmpty) {
          println(s"Extra stats for ${summary.toolName} ($category)")
          val totalDuration   = durations.sum
          val minDuration     = durations.min
          val maxDuration     = durations.max
          val averageDuration = totalDuration / n
          val extraStats =
            runs.map(_.extraStats).filter(_.nonEmpty).map(_.get)
          val searchSpaceSizes = extraStats.map(_.searchSpaceSize)
          val searchSpaceSteps = extraStats.map(_.searchSpaceNumSteps)

          val avgSearchSpaceSizes =
            if (searchSpaceSizes nonEmpty)
              searchSpaceSizes.map(_.last._2).sum / searchSpaceSizes.length
            else 0
          val maxSearchSpaceSize =
            if (searchSpaceSizes nonEmpty)
              searchSpaceSizes.map(_.last._2).max
            else 0
          val avgSearchSpaceSteps =
            if (searchSpaceSteps nonEmpty)
              searchSpaceSteps.map(_.last._2).sum / searchSpaceSteps.length
            else 0
          val maxSearchSpaceSteps =
            if (searchSpaceSizes nonEmpty)
              searchSpaceSteps.map(_.last._2).max
            else 0
          if (durations.nonEmpty) {
            println(s"total benchmarks $totalLength")
            println(f"""Durations ($n solved)
                 |  - Average : $averageDuration%.1f s
                 |  - Min     : $minDuration%.1f s
                 |  - Max     : $maxDuration%.1f s
                 |""".stripMargin)
            if (searchSpaceSizes nonEmpty) {
              println(s"Below values correspond to Inst. space and Inst. steps in Table 3 for $category")
              println("Average search space size : " + avgSearchSpaceSizes)
              println("Max search space size     : " + maxSearchSpaceSize)
              println("Average num search steps  : " + avgSearchSpaceSteps)
              println("Max search steps          : " + maxSearchSpaceSteps)
            }
          }
        }
      }

      println
      println(s"end of $category")
      println("="*80)
      println

    }

    if(printAppendixTable) {
      val endString =
        s"""\\end{longtable}
           | % \\end{table}
        """.stripMargin
      println(appendixTableLines ++ endString)
    }


//    for ((category, toolRuns) <- allToolRuns.groupBy(_._1.category)) {
//      println(s"\\section{${Util.sanitizeString(category)}}")
//      //println(s"\\subsection{${toolRuns._1.toolName}}")
//      println("\\subsection{Overview}")
//      print("\\begin{itemize}\n\\item ")
//      println(Util.sanitizeString(toolRuns.map(_._1).mkString("\n\\item ")))
//      println("\\end{itemize}")
//      println("\\subsection{Results}")
//      //println(s"\\section{$category Results}")
//      println(latexTables(category))
//      if(printCombinatorialResults) {
//        print("\\begin{itemize}\n\\item ")
//        println(combinatorialResults(category).mkString("\n\\item "))
//        println("\\end{itemize}")
//      }
//      println(s"See \\autoref{tbl:${category}-results}.")
//      println
//    }
  }
  def checkIfSameParameters(summaries: Seq[Summary]) = {
    if (summaries.exists(s => s.cpuCount != summaries.head.cpuCount)) {
      printWarning(
        "\t\tRuns were executed on systems with different CPU counts!")
      summaries.foreach(
        summary =>
          printInfo(
            "\t" + summary.toolName + "(" +
              summary.toolVersion + ")" + " on " + summary.startDate + ": " +
              summary.cpuCount + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.architecture != summaries.head.architecture)) {
      printWarning(
        "\t\tRuns were executed on systems with different architectures!")
      summaries.foreach(
        summary =>
          printInfo(
            "\t" + summary.toolName + "(" +
              summary.toolVersion + ")" + " on " + summary.startDate + ": " +
              summary.architecture + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.cpuModel != summaries.head.cpuModel)) {
      printWarning(
        "\t\tRuns were executed on systems with different cpu models!")
      summaries.foreach(
        summary =>
          printInfo(
            "\t" + summary.toolName + "(" +
              summary.toolVersion + ")" + " on " + summary.startDate + ": " +
              summary.cpuModel + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.memTotal != summaries.head.memTotal)) {
      printWarning(
        "\t\tRuns were executed on systems with different total memories!")
      summaries.foreach(
        summary =>
          printInfo(
            "\t" + summary.toolName + "(" +
              summary.toolVersion + ")" + " on " + summary.startDate + ": " +
              summary.memTotal + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.wallTimeLimit != summaries.head.wallTimeLimit)) {
      printWarning(
        "\t\tRuns were executed on systems with different wall time limits!")
      summaries.foreach(
        summary =>
          printInfo(
            "\t" + summary.toolName + ": " +
              summary.wallTimeLimit + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.cpuTimeLimit != summaries.head.cpuTimeLimit)) {
      printWarning(
        "\t\tRuns were executed on systems with different CPU time limits!")
      summaries.foreach(
        summary =>
          printInfo(
            "\t" + summary.toolName + "(" +
              summary.toolVersion + ")" + " on " + summary.startDate + ": " +
              summary.cpuTimeLimit + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.toolVersion != summaries.head.toolVersion)) {
      printWarning(
        "\t\tRuns were executed with different versions of the tool!")
      summaries.foreach(
        summary =>
          printInfo(
            "\t" + summary.toolName + "(" +
              summary.toolVersion + ")" + " on " + summary.startDate + ": " +
              summary.toolVersion + " (" + summary.ymlFileName + ")"))
    }
  }
}
