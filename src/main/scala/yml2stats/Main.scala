package yml2stats

import scala.collection.mutable.{HashSet => MHashSet}
import scala.io.Source
import net.jcazevedo.moultingyaml._
import yml2stats.Benchmarks._
import Benchmarks.MyYamlProtocol._
import yml2stats.parser.{CPAOutputParser, EldaricaOutputParser, SMTExpectedStatusParser, ToolOutputParser, Z3OutputParser}
import java.util.Date

import Settings._

object Main extends App {

  def printWarning(s : String) = {
    if(verbosityLevel >= 1)
      println(s)
  }
  def printInfo(s : String) = {
    if(verbosityLevel >= 2)
      println(s)
  }

  override def main(args: Array[String]): Unit = {
    val usage =
      """Usage: yml2stats inFileName | inDirName
  inFileName      : input file to process
  inDirName       : input directory to process
                    (only files with .yml extension are considered)

e.g., "yml2stats /path/to/dir" will collect all .yml files in dir and produce
      an output.
"""

    if (args.length == 0) {
      println(usage); return
    }
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    def parseOptions(list: List[String]): Unit = {
      list match {
        case Nil => // nothing
        case string :: Nil =>
          inFileName = string
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
      println(inFileName + " not found!"); return
    }

    if(in.isDirectory) {
      printInfo("Processing all .yml files under " + in + "...")
    }

    val files = in.listFiles().toList

////////////////////////////////////////////////////////////////////////////////
// Parse input files and create YAML ASTs

    val yamlAsts = for (file <- files if file.getName.endsWith(".yml")) yield {
      //println(file + "...")

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

    val unmergedToolRuns : Seq[(Summary, RunInfos)] =
      for ((fileName, ast) <- yamlAsts) yield {
        printInfo("Processing " + fileName + "...")
        val (rawSummary, rawRunInfos) =
          ast.convertTo[(SummaryRaw, Seq[RunInfoRaw])]

        val outputParser : ToolOutputParser =
          rawSummary.toolName match { // todo: maybe another method?
            case s if s.toLowerCase contains "eld" => EldaricaOutputParser
            case s if s.toLowerCase contains "z3"  => Z3OutputParser
            case s if s.toLowerCase contains "cpa" => CPAOutputParser
            case s => throw new Exception("An output parser for the tool " +
              s + " is not yet implemented.")
          }

        val expectedStatusParser =
          rawSummary.toolName match { // todo: maybe another method?
            case s if (s.toLowerCase contains "eld") ||
                      (s.toLowerCase contains "z3") ||
                      (s.toLowerCase contains "cpa") => // todo: cpa only if it is printed in the same way as SMT expected status
              SMTExpectedStatusParser
          }

        val runInfos = RunInfos(for (rawRunInfo <- rawRunInfos) yield {
          val result = outputParser(rawRunInfo.toolOutput, rawRunInfo.bmName)
          val expected = expectedStatusParser(rawRunInfo.expected)
          RunInfo(rawRunInfo.bmName, expected, result,
                  rawRunInfo.duration.dropRight(1).toDouble) // todo properly parse duration
        })
        printInfo("done! " + runInfos.length + " runs found.")
        (Summary(rawSummary, fileName), runInfos)
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

    val toolRuns = if(mergeYmlFiles || combineResults) {
      println
      printWarning("Merging files with same tool name and options...")
      val groupedToolRuns =
        if (ignoreDifferentOptions) {
          unmergedToolRuns.groupBy(p =>
            p._1.toolName + (if(ignoreDifferentNotes) "" else p._1.notes))
        } else {
          unmergedToolRuns.groupBy(p =>
            if(ignoreDifferentOptionsForTools contains p._1.toolName)
              p._1.toolName + (if(ignoreDifferentNotes) "" else p._1.notes)
            else
              p._1.toolName +  " (" + p._1.toolOptions + (if(ignoreDifferentNotes) "" else p._1.notes) + ")")
        }
      for ((nameAndOpts, toBeMergedRuns) <- groupedToolRuns) yield {
        // checks to ensure merged files do not differ in any parameters
        if (toBeMergedRuns.length > 1) {
          printWarning("\n\tFound " + toBeMergedRuns.length + " file(s) for " + nameAndOpts)
          val summaries = toBeMergedRuns.map(_._1)
          checkIfSameParameters(summaries)
          val summary = toBeMergedRuns.head._1 // take the summary of the first one
          val allRunsWithDate = toBeMergedRuns.flatMap(p => p._2.runs zip
            p._2.runs.indices.map(_ => p._1.startDate))
          val runsGroupedByBmName: Seq[(String, Seq[(RunInfo, Date)])] =
            allRunsWithDate.groupBy(runs => runs._1.bmBaseName).toSeq
          val uniqueRuns : Seq[RunInfo] =
            for ((name, runsWithDate) <- runsGroupedByBmName) yield {
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
        } else toBeMergedRuns.head
      }
    }
    else unmergedToolRuns

////////////////////////////////////////////////////////////////////////////////
// Fairness checks
    if(printFairnessWarnings) {
      println
      val summaries = toolRuns.map(_._1)
      if (summaries.exists(s => s.cpuCount != summaries.head.cpuCount)) {
        printWarning("Runs were executed on systems with different CPU counts!")
        summaries.foreach(summary => printInfo("\t" + summary.toolName + ": " +
          summary.cpuCount + " (" + summary.ymlFileName + ")"))
      }
      if (summaries.exists(s => s.architecture != summaries.head.architecture)) {
        printWarning("Runs were executed on systems with different architectures!")
        summaries.foreach(summary => printInfo("\t" + summary.toolName + ": " +
          summary.architecture + " (" + summary.ymlFileName + ")"))
      }
      if (summaries.exists(s => s.cpuModel != summaries.head.cpuModel)) {
        printWarning("Runs were executed on systems with different cpu models!")
        summaries.foreach(summary => printInfo("\t" + summary.toolName + ": " +
          summary.cpuModel + " (" + summary.ymlFileName + ")"))
      }
      if (summaries.exists(s => s.memTotal != summaries.head.memTotal)) {
        printWarning("Runs were executed on systems with different total memories!")
        summaries.foreach(summary => printInfo("\t" + summary.toolName + ": " +
          summary.memTotal + " (" + summary.ymlFileName + ")"))
      }
      if (summaries.exists(s => s.wallTimeLimit != summaries.head.wallTimeLimit)) {
        printWarning("Runs were executed on systems with different wall time limits!")
        summaries.foreach(summary => printInfo("\t" + summary.toolName + ": " +
          summary.wallTimeLimit + " (" + summary.ymlFileName + ")"))
      }
      if (summaries.exists(s => s.cpuTimeLimit != summaries.head.cpuTimeLimit)) {
        printWarning("Runs were executed on systems with different CPU time limits!")
        summaries.foreach(summary => printInfo("\t" + summary.toolName + ": " +
          summary.cpuTimeLimit + " (" + summary.ymlFileName + ")"))
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
           if toolRuns.forall(p => p._2.runs.exists(run2 =>
             run2.bmBaseName == run.bmBaseName )))
        yield run.bmBaseName).toSet

    println(commonBenchmarkNames.size + " benchmarks were executed by all tools.")

    val errorRunNamesForEachTool : Seq[Seq[String]] =
      toolRuns.map(p => p._2.errorRuns.filter(run =>
        if(excludeSolverErrors) // leave only errors without solve errors
          !run.result.asInstanceOf[Error].errorTypes.contains(ErrorType.Solve)
        else
          true // do not exclude anything
      ).
        map(_.bmBaseName)).toSeq
    val combinedErrorRuns : Set[String] =
      errorRunNamesForEachTool.reduce(_ union _).toSet
    val commonBenchmarkNamesWithoutErrors : Set[String] =
      commonBenchmarkNames diff combinedErrorRuns

    println
    println(commonBenchmarkNamesWithoutErrors.size +
      " benchmarks had no errors in any of the tools.")

    val commonBenchmarkNamesMaybeWithoutErrors =
      if(excludeErrors) commonBenchmarkNamesWithoutErrors
      else commonBenchmarkNames

    println
    println(
      (if(excludeErrors) "Excluding" else "Including") +
        " benchmarks that any tool reported an error for in comparisons.\n"
    )
    // todo: do not exclude specific types of errors? (e.g., solve)
    //  alternatively categorize these as "unknown"

    val incorrectRunNamesForEachTool : Seq[Seq[String]] =
      toolRuns.map(p => p._2.incorrectRuns.map(_.bmBaseName)).toSeq
    val combinedIncorrectRuns : Set[String] =
      incorrectRunNamesForEachTool.reduce(_ union _).toSet
    val commonBenchmarkNamesWithoutIncorrect : Set[String] =
      commonBenchmarkNamesMaybeWithoutErrors diff combinedIncorrectRuns

    println
    println(commonBenchmarkNamesWithoutIncorrect.size +
      " benchmarks had no incorrect results in any of the tools.")
    println
    println(
      (if(excludeIncorrect) "Excluding" else "Including") +
        " benchmarks that any tool returned an incorrect result for in comparisons.\n"
    )

    val finalCommonBenchmarkNames =
      if(excludeIncorrect) commonBenchmarkNamesWithoutIncorrect
      else commonBenchmarkNamesMaybeWithoutErrors

    //commonBenchmarkNamesWithoutErrors.foreach(println)

    val filteredToolRuns : Seq[(Summary, RunInfos)] =
      (for ((summary, runs) <- toolRuns) yield {
        val filteredRuns = runs.runs.filter(run =>
          finalCommonBenchmarkNames contains run.bmBaseName
        )
        (summary, RunInfos(filteredRuns))
      }).toSeq

    // todo print relevant parts of the summaries of each tool (timeouts etc.)

    val offset = toolRuns.map(_._1.toolName).maxBy(_.length).length
    val tabSpaces = 4
    val columnLabels = Seq("sat(corr.)\t\t", "unsat(corr.)\t", "unknown\t\t", "timeout\t\t", "error\t\t", "correct\t\t", "unsound\t\t", "incomplete\t", "incorrect")
    val firstTabCount = (offset.toDouble / tabSpaces).ceil.toInt + 1
    //val firstTabCount = (minTabCount + (offset.toDouble / tabSpaces).floor.toInt) + 1
    print("\t"*firstTabCount)
    println(columnLabels.mkString(""))
    for((summary, runs) <- filteredToolRuns) {
      val tabsAfterToolName = firstTabCount - (summary.toolName.length.toDouble / tabSpaces).floor.toInt
      print(summary.toolName + "\t"*tabsAfterToolName) // todo: print anything else? notes? version?
      val columns = Seq(runs.satRuns.length + "(" + runs.satRuns.diff(runs.unsoundRuns).length + ")",
        runs.unsatRuns.length + "(" + runs.unsatRuns.diff(runs.incompleteRuns).length + ")",
        runs.unknownRuns.length, runs.timeoutRuns.length, runs.errorRuns.length,
        runs.correctRuns.length, runs.unsoundRuns.length,
        runs.incompleteRuns.length, runs.incorrectRuns.length)
      println(columns.mkString("\t\t\t"))
    }

////////////////////////////////////////////////////////////////////////////////
// Consistency checks
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
            (summary.toolName ,toolRuns.getRun(bmName).get)}
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
////////////////////////////////////////////////////////////////////////////////

// Print combinatorial results (i.e., correct results that a tool had an answer for but a subset of others did not )
    println
    for(((summary, runs), i) <- filteredToolRuns.zipWithIndex) {
      // runs that *only* this tool solved
      var uniqueDiffRuns = runs
      for (j <- filteredToolRuns.indices if i != j) {
        val diffRuns = runs - filteredToolRuns(j)._2 // runs that this tool solved that some other tool could not solve
        println(summary.toolName + " solved " + diffRuns.satRuns.length + "/" +
          diffRuns.unsatRuns.length + " that " +
          filteredToolRuns(j)._1.toolName + " could not solve.")
        uniqueDiffRuns = uniqueDiffRuns - filteredToolRuns(j)._2
      }
      println(summary.toolName + " solved " + uniqueDiffRuns.satRuns.length + "/" +
        uniqueDiffRuns.unsatRuns.length + " that any other tool could not solve.")
      println
    }

// Print combinatorial results (i.e., correct results that a tool had an answer for but a subset of others did not )
    // todo: wip, only here for testing purposes
    if(!disableAllPlots && (plotDurations || plotDurationsFile) &&
      filteredToolRuns.length > 1) {
      println
      println("Generating durations plots")
      for (Seq(toolRuns1, toolRuns2) <- filteredToolRuns.combinations(2)) {
        Plotting.plotDuratıons(toolRuns1, toolRuns2)
      }
    }

    if(!disableAllPlots && (plotCactus || plotCactusFile)) {
      println
      println("Generating cactus plot")
      Plotting.plotCactus(filteredToolRuns)
    }
  }

  private def checkIfSameParameters(summaries: Seq[Summary]) = {
    if (summaries.exists(s => s.cpuCount != summaries.head.cpuCount)) {
      printWarning("\t\tRuns were executed on systems with different CPU counts!")
      summaries.foreach(summary => printInfo("\t" + summary.toolName + "(" +
        summary.toolVersion + ")" + " on " + summary.startDate + ": " +
        summary.cpuCount + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.architecture != summaries.head.architecture)) {
      printWarning("\t\tRuns were executed on systems with different architectures!")
      summaries.foreach(summary => printInfo("\t" + summary.toolName + "(" +
        summary.toolVersion + ")" + " on " + summary.startDate + ": " +
        summary.architecture + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.cpuModel != summaries.head.cpuModel)) {
      printWarning("\t\tRuns were executed on systems with different cpu models!")
      summaries.foreach(summary => printInfo("\t" + summary.toolName + "(" +
        summary.toolVersion + ")" + " on " + summary.startDate + ": " +
        summary.cpuModel + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.memTotal != summaries.head.memTotal)) {
      printWarning("\t\tRuns were executed on systems with different total memories!")
      summaries.foreach(summary => printInfo("\t" + summary.toolName + "(" +
        summary.toolVersion + ")" + " on " + summary.startDate + ": " +
        summary.memTotal + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.wallTimeLimit != summaries.head.wallTimeLimit)) {
      printWarning("\t\tRuns were executed on systems with different wall time limits!")
      summaries.foreach(summary => printInfo("\t" + summary.toolName + ": " +
        summary.wallTimeLimit + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.cpuTimeLimit != summaries.head.cpuTimeLimit)) {
      printWarning("\t\tRuns were executed on systems with different CPU time limits!")
      summaries.foreach(summary => printInfo("\t" + summary.toolName + "(" +
        summary.toolVersion + ")" + " on " + summary.startDate + ": " +
        summary.cpuTimeLimit + " (" + summary.ymlFileName + ")"))
    }
    if (summaries.exists(s => s.toolVersion != summaries.head.toolVersion)) {
      printWarning("\t\tRuns were executed with different versions of the tool!")
      summaries.foreach(summary => printInfo("\t" + summary.toolName + "(" +
        summary.toolVersion + ")" + " on " + summary.startDate + ": " +
        summary.toolVersion + " (" + summary.ymlFileName + ")"))
    }
  }
}