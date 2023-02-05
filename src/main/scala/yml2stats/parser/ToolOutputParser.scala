package yml2stats.parser

import scala.collection.mutable.{HashSet => MHashSet}
import yml2stats.Benchmarks._
import yml2stats.Main
import yml2stats.Settings._

// todo: these parsers need to recognize much more output!

trait ToolOutputParser {
  def apply(outputLines: Seq[String], bmName: String): Result
}
object EldaricaOutputParser extends ToolOutputParser {

  def apply(outputLines: Seq[String], bmName: String): Result = {
    var _isSat         = false
    var _isUnsat       = false
    var _isUnknown     = false
    var _isError       = false
    var _isWallTimeout = false
    var _isKilled      = false

    for (line <- outputLines) {
      line match {
        case _ if line contains "killed" =>
          _isKilled = true;
        case _ if line contains "error" =>
          _isError = true;
        case _ if line contains "unsat" =>
          if (_isSat || _isUnknown)
            throw new Exception(
              bmName + " e1: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnsat = true
        case _ if line contains "sat" =>
          if (_isUnsat || _isUnknown)
            throw new Exception(
              bmName + " e2: cannot determine benchmark output from lines: " + outputLines)
          else
            _isSat = true
        case _ if line contains "unknown" =>
          if (_isSat || _isUnsat)
            throw new Exception(
              bmName + " e3: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnknown = true
        case _ if (line contains "wall timeout") =>
          if (_isSat || _isUnsat) {
            // nothing, it might be that the result returned exactly on timeout
          } else
            _isWallTimeout = true
        case _ => // nothing
      }
    }

    val result: Result =
      if (_isError || _isKilled && !considerKilledAsTimeout) {
        val errorMsg   = outputLines.mkString("\n")
        val errorTypes = new MHashSet[ErrorType.Value]
        for (line <- outputLines) {
          line match {
            case _ if line contains "Predicate generation failed" =>
              errorTypes += ErrorType.Solve
            case _
                if (line contains "OutOfMemoryError") ||
                  (line contains "insufficient memory") ||
                  (line contains "out of memory") =>
              errorTypes += ErrorType.OutOfMemory // todo: detect other errors
            case _ if line contains "error" =>
              errorTypes += ErrorType.Other // todo: detect other errors
            case _ => // nothing
          }
        }
        if (yml2stats.Settings.considerSolveErrorsUnknown &&
            errorTypes.contains(ErrorType.Solve) ||
            yml2stats.Settings.considerOutOfMemErrorUnknown &&
            errorTypes.contains(ErrorType.OutOfMemory))
          Unknown
        else
          Error(errorTypes.toSet, errorMsg)
      } else if (_isSat) {
        True
      } else if (_isUnsat) {
        False
      } else if (_isUnknown) {
        Unknown
      } else if (_isWallTimeout || _isKilled && considerKilledAsTimeout) {
        Timeout
      } else
        throw new Exception(
          bmName + " e5: cannot determine benchmark output from lines: " + outputLines)

    result

  }

}

object Z3OutputParser extends ToolOutputParser {

  def apply(outputLines: Seq[String], bmName: String): Result = {
    var _isSat         = false
    var _isUnsat       = false
    var _isUnknown     = false
    var _isError       = false
    var _isWallTimeout = false
    var _isKilled      = false

    for (line <- outputLines) {
      line match {
        case _ if line contains "killed" =>
          _isKilled = true;
        case _ if line contains "check annotation that says" =>
        // ignore this line
//          println(bmName + "; " + outputLines)
        case _ if (line contains "other error") =>
        // ignore this line
//          println(bmName + "; " + outputLines)
        case _ if (line contains "error") =>
          _isError = true;
        case _ if (line contains "unsat") =>
          if (_isSat || _isUnknown)
            throw new Exception(
              bmName + " e1: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnsat = true
        case _ if (line contains "sat") =>
          if (_isUnsat || _isUnknown)
            throw new Exception(
              bmName + " e2: cannot determine benchmark output from lines: " + outputLines)
          else
            _isSat = true
        case _ if (line contains "unknown") =>
          if (_isSat || _isUnsat)
            throw new Exception(
              bmName + " e3: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnknown = true
        case _ if (line contains "wall timeout") =>
          if (_isSat || _isUnsat) {
            // nothing, it might be that the result returned exactly on timeout
          } else
            _isWallTimeout = true
        case _ => // nothing
      }
    }

    val result: Result =
      if (_isError || _isKilled && !considerKilledAsTimeout) {
        val errorMsg   = outputLines.mkString("\n")
        val errorTypes = new MHashSet[ErrorType.Value]
        for (line <- outputLines) {
          line match {
            case _ if line contains "unknown sort" =>
              errorTypes += ErrorType.Encode
            case _ if line contains "unsupported" =>
              errorTypes += ErrorType.Parse
            case _
                if (line contains "OutOfMemoryError") ||
                  (line contains "insufficient memory") =>
              errorTypes += ErrorType.OutOfMemory // todo: detect other errors
            case _ if line contains "error" =>
              errorTypes += ErrorType.Other // todo: detect other errors
            case _ => // nothing
          }
        }
        if (errorTypes.isEmpty)
          errorTypes += ErrorType.Other
        if (yml2stats.Settings.considerSolveErrorsUnknown &&
            errorTypes.contains(ErrorType.Solve) ||
            yml2stats.Settings.considerOutOfMemErrorUnknown &&
            errorTypes.contains(ErrorType.OutOfMemory))
          Unknown
        else
          Error(errorTypes.toSet, errorMsg)
      } else if (_isSat) {
        True
      } else if (_isUnsat) {
        False
      } else if (_isUnknown) {
        Unknown
      } else if (_isWallTimeout || _isKilled && considerKilledAsTimeout) {
        Timeout
      } else {
        println(
          "Warning: could not determine result for " + bmName +
            ", assuming UNKNOWN. Output lines: " + outputLines)
        Unknown
      }

    result

  }

}

object CPAOutputParser extends ToolOutputParser {

  def apply(outputLines: Seq[String], bmName: String): Result = {
    var _isSat         = false
    var _isUnsat       = false
    var _isUnknown     = false
    var _isError       = false
    var _isWallTimeout = false
    var _isKilled      = false

    for (line <- outputLines) {
      line match {
        case _ if line contains "killed" =>
          _isKilled = true;
        case _ if line contains "Verification result: FALSE" =>
          if (_isSat || _isUnknown)
            throw new Exception(
              bmName + " e1: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnsat = true
        case _ if line contains "Verification result: TRUE" =>
          if (_isUnsat || _isUnknown)
            throw new Exception(
              bmName + " e2: cannot determine benchmark output from lines: " + outputLines)
          else
            _isSat = true
        case _
            if (line contains "Exception in thread") ||
              (line contains "other error") =>
          _isError = true;
        case _ if (line contains "Verification result: UNKNOWN") =>
          if (_isSat || _isUnsat)
            throw new Exception(
              bmName + " e3: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnknown = true
        case _ if (line contains "wall timeout") =>
          if (_isSat || _isUnsat) {
            // nothing, it might be that the result returned exactly on timeout
          } else
            _isWallTimeout = true
        case _ => // nothing
      }
    }

    val result: Result =
      if (_isError || _isKilled && !considerKilledAsTimeout) {
        val errorMsg   = outputLines.mkString("\n")
        val errorTypes = new MHashSet[ErrorType.Value]
        for (line <- outputLines) {
          line match {
            case _ if line contains "unknown sort" =>
              errorTypes += ErrorType.Encode
            case _ if line contains "unsupported" =>
              errorTypes += ErrorType.Parse
            case _
                if (line contains "OutOfMemoryError") ||
                  (line contains "insufficient memory") =>
              errorTypes += ErrorType.OutOfMemory // todo: detect other errors
            case _ if line contains "error" =>
              errorTypes += ErrorType.Other // todo: detect other errors
            case _ => // nothing
          }
        }
        if (errorTypes.isEmpty)
          errorTypes += ErrorType.Other
        if (yml2stats.Settings.considerSolveErrorsUnknown &&
            errorTypes.contains(ErrorType.Solve) ||
            yml2stats.Settings.considerOutOfMemErrorUnknown &&
            errorTypes.contains(ErrorType.OutOfMemory))
          Unknown
        else
          Error(errorTypes.toSet, errorMsg)
      } else if (_isSat) {
        True
      } else if (_isUnsat) {
        False
      } else if (_isUnknown) {
        Unknown
      } else if (_isWallTimeout || _isKilled && considerKilledAsTimeout) {
        Timeout
      } else {
        println(
          "Warning: could not determine result for " + bmName +
            ", assuming UNKNOWN. Output lines: " + outputLines)
        Unknown
      }

//      throw new Exception(
//      bmName + " e5: cannot determine benchmark output from lines: " + outputLines)

    result

  }

}

object SVOutputParser extends ToolOutputParser {

  def apply(outputLines: Seq[String], bmName: String): Result = {
    assert(outputLines.length == 1)

    var _isSat         = false
    var _isUnsat       = false
    var _isUnknown     = false
    var _isError       = false
    var _isWallTimeout = false
    var _isKilled      = false
    val line           = outputLines.head
    line match {
      case _ if line contains "false" =>
        if (_isSat || _isUnknown)
          throw new Exception(
            bmName + " e1: cannot determine benchmark output from lines: " + outputLines)
        else
          _isUnsat = true
      case _ if line contains "true" =>
        if (_isUnsat || _isUnknown)
          throw new Exception(
            bmName + " e2: cannot determine benchmark output from lines: " + outputLines)
        else
          _isSat = true
      case _ if (line.toLowerCase() contains "timeout") =>
        if (_isSat || _isUnsat) {
          // nothing, it might be that the result returned exactly on timeout
        } else
          _isUnknown = true
      case _
          if (line.toLowerCase() contains "error") |
            (line.toLowerCase() contains "segmentation fault") =>
        _isError = true;
      case _ if (line.toLowerCase() contains "unknown") =>
        _isUnknown = true
      case _ if (line.toLowerCase contains "out of memory") =>
        _isUnknown = true
      case _ if (line.toLowerCase contains "out of java memory") =>
        _isUnknown = true
      case _ if (line.toLowerCase contains "exception") =>
        _isError = true
      case _ if (line.toLowerCase contains "assertion") =>
        _isError = true
      case _ if (line.toLowerCase() == "verdict") =>
        _isUnknown = true
      case _ => throw new Exception("Do not know how to parse " + line)
    }

    val result: Result =
      if (_isError || _isKilled && !considerKilledAsTimeout) {
        val errorMsg   = outputLines.mkString("\n")
        val errorTypes = new MHashSet[ErrorType.Value]
        for (line <- outputLines) {
          line match {
            case "ERROR (1)" | "ERROR" | "ERROR (recursion)" | "ERROR (7)" |
                "ERROR (42)" | "SEGMENTATION FAULT" =>
              errorTypes += ErrorType.Other
//          case _ if line contains "unknown sort" =>
//            errorTypes += ErrorType.Encode
            case _
                if (line.toLowerCase contains "out of memory") |
                  (line.toLowerCase contains "out of java memory") =>
              errorTypes += ErrorType.OutOfMemory
            case _ if (line.toLowerCase contains "exception") =>
              errorTypes += ErrorType.Other
            case _ if line contains "parsing failed" =>
              errorTypes += ErrorType.Parse
//          case _ if (line contains "OutOfMemoryError") ||
//            (line contains "insufficient memory") =>
//            errorTypes += ErrorType.OutOfMemory // todo: detect other errors
//          case _ if line contains "ERROR" =>
//            errorTypes += ErrorType.Other // todo: detect other errors
            case _ =>
              Main.printInfo(
                "Classifying as other: do not know how to classify error: " + line)
              errorTypes += ErrorType.Other
            // nothing
          }
        }
        if (errorTypes.isEmpty)
          errorTypes += ErrorType.Other
        if (yml2stats.Settings.considerSolveErrorsUnknown &&
            errorTypes.contains(ErrorType.Solve) ||
            yml2stats.Settings.considerOutOfMemErrorUnknown &&
            errorTypes.contains(ErrorType.OutOfMemory))
          Unknown
        else
          Error(errorTypes.toSet, errorMsg)
      } else if (_isSat) {
        True
      } else if (_isUnsat) {
        False
      } else if (_isUnknown) {
        Unknown
      } else if (_isWallTimeout || _isKilled && considerKilledAsTimeout) {
        Timeout
      } else {
        println(
          "Warning: could not determine result for " + bmName +
            ", assuming UNKNOWN. Output lines: " + outputLines)
        Unknown
      }

    //      throw new Exception(
    //      bmName + " e5: cannot determine benchmark output from lines: " + outputLines)

    result

  }

}

object MonoceraOutputParser extends ToolOutputParser {

  def apply(outputLines: Seq[String], bmName: String): Result = {
    var _isSat         = false
    var _isUnsat       = false
    var _isUnknown     = false
    var _isError       = false
    var _isWallTimeout = false
    var _isKilled      = false

    for (line <- outputLines) {
      line match {
        case _ if line contains "killed" =>
          _isKilled = true;
        case _ if line contains "UNSAFE" =>
          if (_isSat || _isUnknown)
            throw new Exception(
              bmName + " e1: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnsat = true
        case _ if line contains "SAFE" =>
          if (_isUnsat || _isUnknown)
            throw new Exception(
              bmName + " e2: cannot determine benchmark output from lines: " + outputLines)
          else
            _isSat = true
        case _ if (line.toLowerCase contains "error") =>
          _isError = true;
        case _ if (line contains "UNKNOWN") =>
          if (_isSat || _isUnsat)
            throw new Exception(
              bmName + " e3: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnknown = true
        case _ if (line contains "wall timeout") =>
          if (_isSat || _isUnsat) {
            // nothing, it might be that the result returned exactly on timeout
          } else
            _isWallTimeout = true
        case _ =>
        // nothing
      }
    }

    val result: Result =
      if (_isError || _isKilled && !considerKilledAsTimeout) {
        val errorMsg   = outputLines.mkString("\n")
        val errorTypes = new MHashSet[ErrorType.Value]
        for (line <- outputLines) {
          line match {
//          case _ if line contains "unknown sort" =>
//            errorTypes += ErrorType.Encode
            case _ if line contains "Syntax Error" =>
              errorTypes += ErrorType.Parse
            case _ if line contains "theories failed to construct" =>
              errorTypes += ErrorType.Solve
            case _ if (line contains "Out of Memory") =>
              errorTypes += ErrorType.OutOfMemory
            case _ if (line contains "Out of Memory") =>
              errorTypes += ErrorType.OutOfMemory
            case _ if line contains "Other Error" =>
              errorTypes += ErrorType.Other // todo: detect other errors
            case _ => // nothing
          }
        }
        if (errorTypes.isEmpty)
          errorTypes += ErrorType.Other
        if (yml2stats.Settings.considerSolveErrorsUnknown &&
            errorTypes.contains(ErrorType.Solve) ||
            yml2stats.Settings.considerOutOfMemErrorUnknown &&
            errorTypes.contains(ErrorType.OutOfMemory))
          Unknown
        else
          Error(errorTypes.toSet, errorMsg)
      } else if (_isSat) {
        True
      } else if (_isUnsat) {
        False
      } else if (_isUnknown) {
        Unknown
      } else if (_isWallTimeout || _isKilled && considerKilledAsTimeout) {
        Timeout
      } else {
        println(
          "Warning: could not determine result for " + bmName +
            ", assuming UNKNOWN. Output lines: " + outputLines)
        Unknown
      }

//      throw new Exception(
//      bmName + " e5: cannot determine benchmark output from lines: " + outputLines)

    result

  }

}

object SeaHornOutputParser extends ToolOutputParser {

  def apply(outputLines: Seq[String], bmName: String): Result = {
    var _isSat         = false
    var _isUnsat       = false
    var _isUnknown     = false
    var _isError       = false
    var _isWallTimeout = false
    var _isKilled      = false

    for (line <- outputLines) {
      line match {
        case _ if line contains "killed" =>
          _isKilled = true;
        case _ if line contains "unsat" =>
          if (_isUnsat || _isUnknown)
            throw new Exception(
              bmName + " e2: cannot determine benchmark output from lines: "
                + outputLines)
          else
            _isSat = true
        case _ if line contains "sat" =>
          if (_isSat || _isUnknown)
            throw new Exception(
              bmName + " e1: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnsat = true
        case _ if (line.toLowerCase contains "other error") =>
          _isError = true;
//        case _ if (line contains "UNKNOWN") =>
//          if (_isSat || _isUnsat)
//            throw new Exception(
//              bmName + " e3: cannot determine benchmark output from lines: " + outputLines)
//          else
//            _isUnknown = true
        case _ if (line contains "wall timeout") =>
          if (_isSat || _isUnsat) {
            // nothing, it might be that the result returned exactly on timeout
          } else
            _isWallTimeout = true
        case _ =>
        // nothing
      }
    }

    val result: Result =
      if (_isError || _isKilled && !considerKilledAsTimeout) {
        val errorMsg   = outputLines.mkString("\n")
        val errorTypes = new MHashSet[ErrorType.Value]
        for (line <- outputLines) {
          line match {
//          case _ if line contains "unknown sort" =>
//            errorTypes += ErrorType.Encode
            case _ if line contains "Syntax Error" =>
              errorTypes += ErrorType.Parse
            case _ if line contains "theories failed to construct" =>
              errorTypes += ErrorType.Solve
            case _ if (line contains "Out of Memory") =>
              errorTypes += ErrorType.OutOfMemory
            case _ if (line contains "Out of Memory") =>
              errorTypes += ErrorType.OutOfMemory
            case _ if line contains "Other Error" =>
              errorTypes += ErrorType.Other // todo: detect other errors
            case _ => // nothing
          }
        }
        if (errorTypes.isEmpty)
          errorTypes += ErrorType.Other
        if (yml2stats.Settings.considerSolveErrorsUnknown &&
            errorTypes.contains(ErrorType.Solve) ||
            yml2stats.Settings.considerOutOfMemErrorUnknown &&
            errorTypes.contains(ErrorType.OutOfMemory))
          Unknown
        else
          Error(errorTypes.toSet, errorMsg)
      } else if (_isSat) {
        True
      } else if (_isUnsat) {
        False
      } else if (_isUnknown) {
        Unknown
      } else if (_isWallTimeout || _isKilled && considerKilledAsTimeout) {
        Timeout
      } else {
        println(
          "Warning: could not determine result for " + bmName +
            ", assuming UNKNOWN. Output lines: " + outputLines)
        Unknown
      }

//      throw new Exception(
//      bmName + " e5: cannot determine benchmark output from lines: " + outputLines)

    result

  }

}
