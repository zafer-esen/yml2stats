package yml2stats

import net.jcazevedo.moultingyaml._

object Benchmarks {
  import Parameters._

  object ErrorType extends Enumeration {
    type ErrorType = Value
    val Parse, Encode, Solve, Other = Value
  }

  case class SummaryRaw(toolName      : String,
                        toolVersion   : String,
                        toolOptions   : String,
                        cpuTimeLimit  : String,
                        wallTimeLimit : String,
                        os            : String,
                        cpuModel      : String,
                        cpuCount      : String,
                        architecture  : String,
                        memTotal      : String, // todo: int in MB?
                        startDate     : String,
                        scriptDir     : String,
                        notes         : String)

  case class RunInfoRaw(bmName     : String,
                        expected   : String,
                        toolOutput : Seq[String],
                        duration   : String)

  object MyYamlProtocol extends DefaultYamlProtocol {
    implicit val summaryFormat = yamlFormat13(SummaryRaw)
    implicit val runInfoFormat = yamlFormat4(RunInfoRaw)
  }

  trait Result
  case object True extends Result
  case object False extends Result
  case object Timeout extends Result
  case object Unknown extends Result
  case class Error(errorTypes : Set[ErrorType.Value],
                   errorMsg : String) extends Result {
    override def toString: String =
      if (errorTypes.nonEmpty)
        "(" + errorTypes.mkString(", ") + ")"
      else ""
  }


  case class RunInfo(bmName     : String,
                     expected   : Result,
                     result     : Result,
                     duration   : Double) {
    //name without ext where bmName = dir/bmBaseName.ext
    val bmBaseName : String = {
      if(discardBenchmarkExtensions) {
        var curName = bmName.split("/").last
        var prevCurName = ""

        while (curName != prevCurName) {
          prevCurName = curName
          for (ext <- benchmarkExtensions) {
            curName = curName.stripSuffix(ext)
          }
        }
        curName
      } else bmName
    }
  }

  object RunInfos {
    def apply(runs : Seq[RunInfo]) : RunInfos =
      new RunInfos(
        runs,
        runs.filter(run => run.result == True),
        runs.filter(run => run.result == False),
        runs.filter(run => run.result == Unknown),
        runs.filter(run => run.result.isInstanceOf[Error]), // todo: categorize (parse/encode etc.)?
        runs.filter(run => run.result == Timeout) // todo: categorize (wall/cpu)?
      )
  }

  class RunInfos(val runs : Seq[RunInfo],
                 val satRuns : Seq[RunInfo],
                 val unsatRuns : Seq[RunInfo],
                 val unknownRuns : Seq[RunInfo],
                 val errorRuns : Seq[RunInfo],
                 val timeoutRuns : Seq[RunInfo]) {
    val length      = runs.length
    private def diffByBaseName (a : Seq[RunInfo],
                                b : Seq[RunInfo]) : Seq[RunInfo] = {
      val diffNames = (a.map(_.bmBaseName) diff b.map(_.bmBaseName))
      a.filter(run => diffNames contains run.bmBaseName)
    }

    def -(that: RunInfos): RunInfos = {
      val diffRuns = diffByBaseName(runs, that.runs)
      val diffSatRuns = diffByBaseName(satRuns, that.satRuns)
      val diffUnsatRuns = diffByBaseName(unsatRuns, that.unsatRuns)
      val diffUnknownRuns = diffByBaseName(unknownRuns, that.unknownRuns)
      val diffErrorRuns = diffByBaseName(errorRuns, that.errorRuns)
      val diffTimeoutRuns = diffByBaseName(timeoutRuns, that.timeoutRuns)
      new RunInfos(diffRuns, diffSatRuns, diffUnsatRuns, diffUnknownRuns,
        diffErrorRuns, diffTimeoutRuns)
    }

    private def errorsToString = {
      val groupedErrors = errorRuns.groupBy(run =>
        run.result.asInstanceOf[Error].errorTypes)
      if (groupedErrors.nonEmpty) {
        "(" + (for ((_, runs) <- groupedErrors) yield {
          runs.head.result.toString + ": " + runs.length
        }).mkString("; ") + ")"
      } else ""
    }

    override def toString: String = {
      "sat     : " + satRuns.length + "\n" +
      "unsat   : " + unsatRuns.length + "\n" +
      "unknown : " + unknownRuns.length + "\n" +
      "timeout : " + timeoutRuns.length + "\n" +
      "error   : " + errorRuns.length + " " + errorsToString + "\n" +
      "total   : " + runs.length
    }
  }

  object Summary {
    def apply(raw : SummaryRaw, fileName : String) : Summary = {
      Summary(
        raw.toolName,
        raw.toolVersion,
        raw.toolOptions,
        raw.cpuTimeLimit.dropRight(1).toDouble,
        raw.wallTimeLimit.dropRight(1).toDouble,
        raw.os,
        raw.cpuModel,
        raw.cpuCount.toInt,
        raw.architecture,
        raw.memTotal,
        raw.startDate,
        raw.scriptDir,
        raw.notes,
        fileName
      )
    }
  }
  case class Summary(toolName      : String,
                     toolVersion   : String,
                     toolOptions   : String,
                     cpuTimeLimit  : Double,
                     wallTimeLimit : Double,
                     os            : String,
                     cpuModel      : String,
                     cpuCount      : Int,
                     architecture  : String,
                     memTotal      : String, // todo: int in MB?
                     startDate     : String,
                     scriptDir     : String,
                     notes         : String,
                     ymlFileName   : String) {
    override def toString: String =
      toolName + " (" + toolVersion + ") on " + startDate +
        (if(notes.nonEmpty) (" (" + notes + ")") else "") +
        " (" + ymlFileName + ")"
  }
}
