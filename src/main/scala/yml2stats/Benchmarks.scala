package yml2stats

import net.jcazevedo.moultingyaml._

object Benchmarks {
  import Settings._

  object ErrorType extends Enumeration {
    type ErrorType = Value
    val Parse, Encode, Solve, OutOfMemory, Other = Value
  }

  case class SummaryRaw(toolName:      String,
                        toolVersion:   String,
                        toolOptions:   String,
                        cpuTimeLimit:  String,
                        wallTimeLimit: String,
                        os:            String,
                        cpuModel:      String,
                        cpuCount:      String,
                        architecture:  String,
                        memTotal:      String, // todo: int in MB?
                        startDate:     String,
                        scriptDir:     String,
                        notes:         String)

  case class RunInfoRaw(bmName:     String,
                        expected:   String,
                        toolOutput: Seq[String],
                        duration:   String)

  object MyYamlProtocol extends DefaultYamlProtocol {
    implicit val summaryFormat = yamlFormat13(SummaryRaw)
    implicit val runInfoFormat = yamlFormat4(RunInfoRaw)
  }

  trait Result
  case object True    extends Result
  case object False   extends Result
  case object Timeout extends Result
  case object Unknown extends Result
  case class Error(errorTypes: Set[ErrorType.Value], errorMsg: String)
      extends Result {
    override def toString: String =
      if (errorTypes.nonEmpty)
        "(" + errorTypes.mkString(", ") + errorMsg.replace("\n", ";") + ")"
      else ""
  }

  case class RunInfo(bmName:   String,
                     expected: Result,
                     result:   Result,
                     duration: Double,
                     category: String) {
    //name without ext where bmName = dir/bmBaseName.ext
    val bmBaseName: String = {
      if (discardBenchmarkExtensions) {
        var curName     = bmName.split("/").last
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
    def apply(runs: Seq[RunInfo]): RunInfos = {
      val sortedRuns: Seq[RunInfo] =
        if (Settings.uniqueBenchmarks)
          runs
            .groupBy(_.bmBaseName)
            .map(_._2.head)
            .toSeq
            .sortBy(_.bmBaseName) // take first if multiple runs exist with same bmBaseName
        else runs.sortBy(_.bmBaseName)
      new RunInfos(
        sortedRuns,
        sortedRuns.filter(run => run.result == True),
        sortedRuns.filter(run => run.result == False),
        sortedRuns.filter(run => run.result == Unknown),
        sortedRuns
          .filter(run => run.result.isInstanceOf[Error]), // todo: categorize (parse/encode etc.)?
        sortedRuns
          .filter(run => run.result == Timeout) // todo: categorize (wall/cpu)?
      )
    }
  }

  // note: runs should be sorted!
  class RunInfos(val runs:        Seq[RunInfo],
                 val satRuns:     Seq[RunInfo],
                 val unsatRuns:   Seq[RunInfo],
                 val unknownRuns: Seq[RunInfo],
                 val errorRuns:   Seq[RunInfo],
                 val timeoutRuns: Seq[RunInfo]) {
    val length = runs.length

    val correctRuns =
      (satRuns ++ unsatRuns).filter(run => run.result == run.expected)
    val unsoundRuns =
      satRuns.filter(run => run.expected == False)
    val incompleteRuns =
      unsatRuns.filter(run => run.expected == True)
    def incorrectRuns = unsoundRuns ++ incompleteRuns

    def getRun(bmBaseName: String): Option[RunInfo] = {
      runs.find(run => run.bmBaseName == bmBaseName)
    }

    private def diffByBaseName(a: Seq[RunInfo],
                               b: Seq[RunInfo]): Seq[RunInfo] = {
      val diffNames = (a.map(_.bmBaseName) diff b.map(_.bmBaseName))
      a.filter(run => diffNames contains run.bmBaseName)
    }

    def -(that: RunInfos): RunInfos = {
      val diffRuns        = diffByBaseName(runs, that.runs)
      val diffSatRuns     = diffByBaseName(satRuns, that.satRuns)
      val diffUnsatRuns   = diffByBaseName(unsatRuns, that.unsatRuns)
      val diffUnknownRuns = diffByBaseName(unknownRuns, that.unknownRuns)
      val diffErrorRuns   = diffByBaseName(errorRuns, that.errorRuns)
      val diffTimeoutRuns = diffByBaseName(timeoutRuns, that.timeoutRuns)
      new RunInfos(diffRuns,
                   diffSatRuns,
                   diffUnsatRuns,
                   diffUnknownRuns,
                   diffErrorRuns,
                   diffTimeoutRuns)
    }

    private def errorsToString = {
      val groupedErrors =
        errorRuns.groupBy(run => run.result.asInstanceOf[Error].errorTypes)
      if (groupedErrors.nonEmpty) {
        "(" + (for ((_, runs) <- groupedErrors) yield {
          runs.head.result.toString + ": " + runs.length
        }).mkString("; ") + ")"
      } else ""
    }

    override def toString: String = {
      "sat       : " + satRuns.length + "\n" +
        "unsat     : " + unsatRuns.length + "\n" +
        "unknown   : " + unknownRuns.length + "\n" +
        "timeout   : " + timeoutRuns.length + "\n" +
        "error     : " + errorRuns.length + " " + errorsToString + "\n" +
        "correct   : " + correctRuns.length + "\n" +
        "incorrect : " + incorrectRuns.length + "\n" +
        "  unsound : " + unsoundRuns.length + "\n" +
        "  incomp. : " + incompleteRuns.length + "\n" +
        "total     : " + runs.length
    }
  }

  object Summary {
    def apply(raw:      SummaryRaw,
              fileName: String,
              category: String = ""): Summary = {
      val toolNameSplit = raw.toolName.split("/")
      val toolName: String =
        (if (toolNameSplit.length > 1) toolNameSplit.last
         else raw.toolName)
          .stripSuffix(".out")
          .stripSuffix("-memsafety")
          .stripSuffix("-reach")
      Summary(
        toolName,
        raw.toolVersion,
        raw.toolOptions,
        raw.cpuTimeLimit.dropRight(1).toDouble,
        raw.wallTimeLimit.dropRight(1).toDouble,
        raw.os,
        raw.cpuModel,
        raw.cpuCount.toInt,
        raw.architecture,
        raw.memTotal,
        dateFormat.parse(raw.startDate),
        raw.scriptDir,
        raw.notes,
        fileName,
        category
      )
    }
  }
  case class Summary(toolName:      String,
                     toolVersion:   String,
                     toolOptions:   String,
                     cpuTimeLimit:  Double,
                     wallTimeLimit: Double,
                     os:            String,
                     cpuModel:      String,
                     cpuCount:      Int,
                     architecture:  String,
                     memTotal:      String, // todo: int in MB?
                     startDate:     java.util.Date,
                     scriptDir:     String,
                     notes:         String,
                     ymlFileName:   String,
                     category:      String) {
    override def toString: String =
      toolName + " (" + toolVersion + ") started on " + startDate +
        (if (category.nonEmpty) (" (" + category + ")") else "") +
        (if (notes.nonEmpty) (" (" + notes + ")") else "") +
        (if (toolOptions.nonEmpty) (" using options: \"" + toolOptions + "\"")
         else "") +
        //" (" + ymlFileName + ")" +
        s" timelimit-cpu: $cpuTimeLimit timelimit-wall: $wallTimeLimit)"
  }
}
