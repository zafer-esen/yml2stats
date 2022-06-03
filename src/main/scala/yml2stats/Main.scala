package yml2stats

import scala.collection.mutable.{HashSet => MHashSet}

import scala.io.Source
import net.jcazevedo.moultingyaml._
import yml2stats.Benchmarks._
import Benchmarks.MyYamlProtocol._
import yml2stats.parser.{EldaricaOutputParser, SMTExpectedStatusParser, ToolOutputParser, Z3OutputParser}

object Main extends App {

  import Parameters._

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
      println("Processing all .yml files under " + in + "...")
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

////////////////////////////////////////////////////////////////////////////////
// Convert YAML ASTs into useful data structures

    val toolRuns : Seq[(Summary, RunInfos)] =
      for ((fileName, ast) <- yamlAsts) yield {
        print("Processing " + fileName + "...")
        val (rawSummary, rawRunInfos) =
          ast.convertTo[(SummaryRaw, Seq[RunInfoRaw])]

        val outputParser : ToolOutputParser =
          rawSummary.toolName match { // todo: maybe another method?
            case s if s.toLowerCase contains "eld" => EldaricaOutputParser
            case s if s.toLowerCase contains "z3"  => Z3OutputParser
          }

        val expectedStatusParser =
          rawSummary.toolName match { // todo: maybe another method?
            case s if (s.toLowerCase contains "eld") ||
                      (s.toLowerCase contains "z3") => SMTExpectedStatusParser
          }

        val runInfos = RunInfos(for (rawRunInfo <- rawRunInfos) yield {
          val result = outputParser(rawRunInfo.toolOutput)
          val expected = expectedStatusParser(rawRunInfo.expected)
          RunInfo(rawRunInfo.bmName, expected, result,
                  rawRunInfo.duration.dropRight(1).toDouble) // todo properly parse duration
        })
        println("done! " + runInfos.length + " runs found.")
        (Summary(rawSummary, fileName), runInfos)
      }

////////////////////////////////////////////////////////////////////////////////
// Fairness checks
    if(printFairnessWarnings) {
      println
      val summaries = toolRuns.map(_._1)
      if (summaries.exists(s => s.cpuCount != summaries.head.cpuCount)) {
        println("Warning: runs were executed on systems with different CPU counts!")
      }
      if (summaries.exists(s => s.architecture != summaries.head.architecture)) {
        println("Warning: runs were executed on systems with different architectures!")
      }
      if (summaries.exists(s => s.cpuModel != summaries.head.cpuModel)) {
        println("Warning: runs were executed on systems with different cpu models!")
      }
      if (summaries.exists(s => s.memTotal != summaries.head.memTotal)) {
        println("Warning: runs were executed on systems with different total memories!")
      }
      if (summaries.exists(s => s.wallTimeLimit != summaries.head.wallTimeLimit)) {
        println("Warning: runs were executed on systems with different wall time limits!")
      }
      if (summaries.exists(s => s.cpuTimeLimit != summaries.head.cpuTimeLimit)) {
        println("Warning: runs were executed on systems with different CPU time limits!")
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
      toolRuns.map(p => p._2.errorRuns.map(_.bmBaseName))
    val combinedErrorRuns : Set[String] =
      errorRunNamesForEachTool.reduce(_ union _).toSet
    val commonBenchmarkNamesWithoutErrors : Set[String] =
      commonBenchmarkNames diff combinedErrorRuns

    println
    println(commonBenchmarkNamesWithoutErrors.size +
      " benchmarks had no errors in any of the tools.")

    println
    println(
      (if(excludeErrors) "Excluding" else "Including") +
        " benchmarks that any tool reported an error for in comparisons.\n"
    )
    // todo: do not exclude specific types of errors? (e.g., solve)
    //  alternatively categorize these as "unknown"

    //commonBenchmarkNamesWithoutErrors.foreach(println)

    val filteredToolRuns =
      for ((summary, runs) <- toolRuns) yield {
        val filteredRuns = runs.runs.filter(run =>
          if (excludeErrors)
            commonBenchmarkNamesWithoutErrors contains run.bmBaseName
          else
            commonBenchmarkNames contains run.bmBaseName
        )
        (summary, RunInfos(filteredRuns))
      }

    // todo print relevant parts of the summaries of each tool (timeouts etc.)

    val offset = toolRuns.map(_._1.toolName).maxBy(_.length).length
    val tabSpaces = 4
    val columnLabels = Seq("sat\t\t\t", "unsat\t\t", "unknown\t\t", "timeout\t\t", "error")
    val firstTabCount = (offset.toDouble / tabSpaces).ceil.toInt + 1
    //val firstTabCount = (minTabCount + (offset.toDouble / tabSpaces).floor.toInt) + 1
    print("\t"*firstTabCount)
    println(columnLabels.mkString(""))
    for((summary, runs) <- filteredToolRuns) {
      val tabsAfterToolName = firstTabCount - (summary.toolName.length.toDouble / tabSpaces).floor.toInt
      print(summary.toolName + "\t"*tabsAfterToolName) // todo: print anything else? notes? version?
      val columns = Seq(runs.satRuns.length, runs.unsatRuns.length,
        runs.unknownRuns.length, runs.timeoutRuns.length, runs.errorRuns.length)
      println(columns.mkString("\t\t\t"))
    }

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
  }
}