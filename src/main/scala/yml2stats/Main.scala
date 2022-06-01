package yml2stats

import java.io.{BufferedWriter, FileWriter}

import scala.io.Source
import net.jcazevedo.moultingyaml._
import yml2stats.Benchmarks.{RunInfo, RunInfoRaw, Summary, SummaryRaw}
import Benchmarks.MyYamlProtocol._
import yml2stats.parser.{EldaricaOutputParser, SMTExpectedStatusParser, ToolOutputParser, Z3OutputParser}

object Main extends App {
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

    // default args
    var inFileName = ""

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

    val toolRuns : Seq[(Summary, Seq[RunInfo])] =
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

        val runInfos = for (rawRunInfo <- rawRunInfos) yield {
          val result = outputParser(rawRunInfo.toolOutput)
          val expected = expectedStatusParser(rawRunInfo.expected)
          RunInfo(rawRunInfo.bmName, expected, result,
                  rawRunInfo.duration.dropRight(1).toDouble) // todo properly parse duration
        }
        println("done! " + runInfos.length + " runs found.")
        (Summary(rawSummary), runInfos)
      }

////////////////////////////////////////////////////////////////////////////////
// Fairness checks
    // timeout
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

////////////////////////////////////////////////////////////////////////////////

  }
}