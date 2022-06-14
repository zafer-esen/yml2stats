package yml2stats.parser

import scala.collection.mutable.{HashSet => MHashSet}
import yml2stats.Benchmarks._

// todo: these parsers need to recognize much more output!

trait ToolOutputParser {
  def apply(outputLines : Seq[String]) : Result
}
object EldaricaOutputParser extends ToolOutputParser {

  def apply(outputLines : Seq[String]) : Result = {
    var _isSat = false
    var _isUnsat = false
    var _isUnknown = false
    var _isError = false
    var _isWallTimeout = false

    for (line <- outputLines) {
      line match {
        case _ if line contains "error" =>
          _isError = true;
        case _ if line contains "unsat" =>
          if (_isSat || _isUnknown) throw new Exception(
            "e1: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnsat = true
        case _ if line contains "sat" =>
          if (_isUnsat || _isUnknown) throw new Exception(
            "e2: cannot determine benchmark output from lines: " + outputLines)
          else
            _isSat = true
        case _ if line contains "unknown" =>
          if (_isSat || _isUnsat) throw new Exception(
            "e3: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnknown = true
        case _ if (line contains "wall timeout")=>
          if (_isSat || _isUnsat) {
            // nothing, it might be that the result returned exactly on timeout
          }
          else
            _isWallTimeout = true
        case _ => // nothing
      }
    }

    val result: Result = if (_isError) {
      val errorMsg = outputLines.mkString("\n")
      val errorTypes = new MHashSet[ErrorType.Value]
      for(line <- outputLines) {
        line match {
          case _ if line contains "Predicate generation failed" =>
            errorTypes += ErrorType.Solve
          case _ =>
            errorTypes += ErrorType.Other // todo: detect other errors
          case _ if line contains "error" =>
            errorTypes += ErrorType.Other // todo: detect other errors
          case _ => // nothing
        }
      }
      Error(errorTypes.toSet, errorMsg)
    } else if (_isSat) {
      True
    } else if (_isUnsat) {
      False
    } else if (_isUnknown) {
      Unknown
    } else if (_isWallTimeout) {
      Timeout
    } else throw new Exception(
      "e5: cannot determine benchmark output from lines: " + outputLines)

    result

  }

}

object Z3OutputParser extends ToolOutputParser {

  def apply(outputLines : Seq[String]) : Result = {
    var _isSat = false
    var _isUnsat = false
    var _isUnknown = false
    var _isError = false
    var _isWallTimeout = false
    var _isKilled = false

    for (line <- outputLines) {
      line match {
        case _ if line contains "killed" =>
          _isKilled = true;
        case _ if line contains "error" =>
          _isError = true;
        case _ if line contains "unsat" =>
          if (_isSat || _isUnknown) throw new Exception(
            "e1: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnsat = true
        case _ if line contains "sat" =>
          if (_isUnsat || _isUnknown) throw new Exception(
            "e2: cannot determine benchmark output from lines: " + outputLines)
          else
            _isSat = true
        case _ if (line contains "unknown")=>
          if (_isSat || _isUnsat) throw new Exception(
            "e3: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnknown = true
        case _ if (line contains "wall timeout")=>
          if (_isSat || _isUnsat) {
            // nothing, it might be that the result returned exactly on timeout
          }
          else
            _isWallTimeout = true
        case _ => // nothing
      }
    }

    val result: Result = if (_isError || _isKilled) {
      val errorMsg = outputLines.mkString("\n")
      val errorTypes = new MHashSet[ErrorType.Value]
      for(line <- outputLines) {
        line match {
          case _ if line contains "unknown sort" =>
            errorTypes += ErrorType.Encode
          case _ if line contains "unsupported" =>
            errorTypes += ErrorType.Parse
          case _ if line contains "error" =>
            errorTypes += ErrorType.Other // todo: detect other errors
          case _ if line contains "out of memory" =>
            errorTypes += ErrorType.OutOfMemory // todo: detect other errors
          case _ => // nothing
        }
      }
      if (errorTypes.isEmpty)
        errorTypes += ErrorType.Other
      Error(errorTypes.toSet, errorMsg)
    } else if (_isSat) {
      True
    } else if (_isUnsat) {
      False
    } else if (_isUnknown) {
      Unknown
    } else if (_isWallTimeout) {
      Timeout
    } else throw new Exception(
      "e5: cannot determine benchmark output from lines: " + outputLines)

    result

  }

}

object CPAOutputParser extends ToolOutputParser {

  def apply(outputLines : Seq[String]) : Result = {
    var _isSat = false
    var _isUnsat = false
    var _isUnknown = false
    var _isError = false
    var _isWallTimeout = false
    var _isKilled = false

    for (line <- outputLines) {
      line match {
        case _ if line contains "killed" =>
          _isKilled = true;
        case _ if line contains "Verification result: FALSE" =>
          if (_isSat || _isUnknown) throw new Exception(
            "e1: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnsat = true
        case _ if line contains "Verification result: TRUE" =>
          if (_isUnsat || _isUnknown) throw new Exception(
            "e2: cannot determine benchmark output from lines: " + outputLines)
          else
            _isSat = true
        case _ if line contains "error" =>
          _isError = true;
        case _ if (line contains "Verification result: UNKNOWN")=>
          if (_isSat || _isUnsat) throw new Exception(
            "e3: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnknown = true
        case _ if (line contains "wall timeout")=>
          if (_isSat || _isUnsat) {
            // nothing, it might be that the result returned exactly on timeout
          }
          else
            _isWallTimeout = true
        case _ => // nothing
      }
    }

    val result: Result = if (_isError || _isKilled) {
      val errorMsg = outputLines.mkString("\n")
      val errorTypes = new MHashSet[ErrorType.Value]
      for(line <- outputLines) {
        line match {
          case _ if line contains "unknown sort" =>
            errorTypes += ErrorType.Encode
          case _ if line contains "unsupported" =>
            errorTypes += ErrorType.Parse
          case _ if line contains "error" =>
            errorTypes += ErrorType.Other // todo: detect other errors
          case _ if line contains "out of memory" =>
            errorTypes += ErrorType.OutOfMemory // todo: detect other errors
          case _ => // nothing
        }
      }
      if (errorTypes.isEmpty)
        errorTypes += ErrorType.Other
      Error(errorTypes.toSet, errorMsg)
    } else if (_isSat) {
      True
    } else if (_isUnsat) {
      False
    } else if (_isUnknown) {
      Unknown
    } else if (_isWallTimeout) {
      Timeout
    } else throw new Exception(
      "e5: cannot determine benchmark output from lines: " + outputLines)

    result

  }

}