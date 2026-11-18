package yml2stats

import org.scalatest.FunSuite
import yml2stats.Benchmarks._
import yml2stats.GroupTables.RuntimeComparison

class RuntimesSpec extends FunSuite {
  private def runs(results : (String, Result, Double)*) : RunInfos =
    RunInfos(results.map { case (name, result, duration) =>
      RunInfo(name, Unknown, result, duration) })

  test("runtimes: ratios on the benchmarks solved by both with the verdict") {
    val base = runs(("p1", True, 1.0), ("p2", True, 1.0), ("p3", True, 1.0),
                    ("p4", True, 1.0), ("p5", True, 1.0), ("p6", False, 1.0))
    val tool = runs(("p1", True, 0.25), ("p2", True, 1.0), ("p3", True, 4.0),
                    ("p4", True, 4.0), ("p5", Timeout, 10.0), ("p6", True, 1.0))
    val sat = GroupTables.runtimeComparison(tool, base, _.correctSatRuns)
    assert((sat.both, sat.faster, sat.slower, sat.median) == (4, 1, 2, Some(2.5)))
    assert(math.abs(sat.geometricMean.get - math.sqrt(2)) < 1e-9)
    assert(GroupTables.runtimeComparison(tool, base, _.correctUnsatRuns) ==
             RuntimeComparison(0, 0, 0, None, None))
  }
}
