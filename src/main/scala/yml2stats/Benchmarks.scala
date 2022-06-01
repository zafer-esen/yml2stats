package yml2stats

import net.jcazevedo.moultingyaml._

object Benchmarks {
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
                   errorMsg : String) extends Result


  case class RunInfo(bmName     : String,
                     expected   : Result,
                     result     : Result,
                     duration   : Double)

  object Summary {
    def apply(raw : SummaryRaw) : Summary = {
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
        raw.notes
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
                     notes         : String)

  // implicit converters for YAML
  //  object MyYamlProtocol extends DefaultYamlProtocol {
  //
  //    implicit object ErrorTypeYamlFormat extends YamlFormat[ErrorType.Value] {
  //      def write(c: ErrorType.Value) = YamlString(c.toString)
  //      def read(value: YamlValue) = value match {
  //        case YamlString(name) =>
  //          name match {
  //            case "Parse" => ErrorType.Parse
  //            case "Encode" => ErrorType.Encode
  //            case "Solve" => ErrorType.Solve
  //            case "Other" => ErrorType.Other
  //          }
  //        case _ => deserializationError("Color expected")
  //      }
  //    }
  //
  //    implicit object ResultYamlFormat extends YamlFormat[Result] {
  //      def write(c: Result) =
  //        YamlArray(
  //          YamlString(c.getClass.getSimpleName), //name
  //          c match {
  //            case Error(errorTypes, errorMsg) =>
  //              YamlArray(errorTypes.toList.toYaml, errorMsg.toYaml) // error types and msg
  //            case _ =>
  //              YamlNull
  //          }
  //        )
  //      def read(value: YamlValue) = value match {
  //        case YamlArray(
  //        Vector(
  //        YamlString(name),
  //        YamlNull)) =>
  //          name match {
  //            case "True"    => True
  //            case "False"   => False
  //            case "Timeout" => Timeout
  //            case "Unknown" => Unknown
  //            case "Error"   => Error(Set(), "")
  //          }
  //        case YamlArray(
  //        Vector(
  //        YamlString(name),
  //        YamlArray(
  //        Vector(
  //          YamlArray(errorTypes),
  //          YamlString(errorMsg)
  //        )
  //        ))) =>
  //          name match {
  //            case "True"    => True
  //            case "False"   => False
  //            case "Timeout" => Timeout
  //            case "Unknown" => Unknown
  //            case "Error"   =>
  //              Error(errorTypes.map(e => e.convertTo[ErrorType.Value]).toSet,
  //                    errorMsg)
  //          }
  //        case _ => deserializationError("Color expected")
  //      }
  //    }
  //  }
}
