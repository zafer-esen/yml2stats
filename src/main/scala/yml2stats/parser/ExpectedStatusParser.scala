package yml2stats.parser

import scala.collection.mutable.{HashSet => MHashSet}
import yml2stats.Benchmarks._

// todo: add expected status parser for SV-Comp .yml files

trait ExpectedStatusParser {
  def apply(expectedStatusLine: String): Result
}
object SMTExpectedStatusParser extends ExpectedStatusParser {

  def apply(expectedStatusLine: String): Result = {
    expectedStatusLine match {
      case s if s.toLowerCase contains "unsat" => False
      case s if s.toLowerCase contains "sat"   => True
      case _                                   => Unknown
    }
  }

}

object SVExpectedStatusParser extends ExpectedStatusParser {

  def apply(expectedStatusLine: String): Result = {
    expectedStatusLine match {
      case s if s.toLowerCase contains "false"   => False
      case s if s.toLowerCase contains "true"    => True
      case s if s.toLowerCase contains "unknown" => Unknown
      case _ =>
        println(expectedStatusLine)
        ???
    }
  }
}

object MonoceraExpectedStatusParser extends ExpectedStatusParser {

  def apply(expectedStatusLine: String): Result = {
    expectedStatusLine match {
      case s if s.toLowerCase contains "UNSAFE" => False
      case s if s.toLowerCase contains "SAFE"   => True
      case s if s.toLowerCase contains "UNKOWN" => Unknown
      case s if s.toLowerCase == ""             => True
      case _ =>
        println(expectedStatusLine)
        ???
    }
  }
}
