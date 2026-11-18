package yml2stats

import org.scalatest.FunSuite
import yml2stats.Benchmarks._

class ScatterSpec extends FunSuite {
  private def runs(results : (String, Result, Result, Double)*) : RunInfos =
    RunInfos(results.map { case (name, expected, result, duration) =>
      RunInfo(name, expected, result, duration) })

  test("scatter: the benchmarks solved by one of two tools, unsolved at the limit") {
    val tool = runs(("p1", True, True, 1.0), ("p2", False, False, 1.0),
                    ("p3", False, True, 1.0), ("p4", True, Unknown, 1.0),
                    ("p5", True, True, 1.0), ("p6", True, Timeout, 10.0))
    val other = runs(("p1", True, True, 2.0), ("p2", False, Unknown, 2.0),
                     ("p3", False, False, 2.0), ("p4", True, True, 2.0),
                     ("p5", True, Unknown, 2.0), ("p6", True, Timeout, 10.0))
    assert(Plotting.scatterPoints(tool, other, 10.0) ==
             Map("sat" -> Seq(("p1", 1.0, 2.0), ("p4", 10.0, 2.0),
                              ("p5", 1.0, 10.0)),
                 "unsat" -> Seq(("p2", 1.0, 10.0), ("p3", 10.0, 2.0))))
  }

  test("scatter: a benchmark with different verdicts of the two tools is inconsistent") {
    val tool = runs(("p1", Unknown, True, 1.0), ("p2", Unknown, True, 1.0))
    val other = runs(("p1", Unknown, False, 2.0), ("p2", Unknown, True, 2.0))
    assert(Plotting.scatterPoints(tool, other, 10.0) ==
             Map("sat" -> Seq(("p2", 1.0, 2.0)),
                 Plotting.Inconsistent -> Seq(("p1", 1.0, 2.0))))
  }
}
