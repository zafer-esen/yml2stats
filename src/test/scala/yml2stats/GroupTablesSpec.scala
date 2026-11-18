package yml2stats

import org.scalatest.FunSuite
import yml2stats.Benchmarks._

// the group g1: tools a, b, c on p1 to p5; a answers sat on p3, expected unsat
class GroupTablesSpec extends FunSuite {
  private val ymlFiles =
    new java.io.File("src/test/resources/g1").listFiles.toList.sortBy(_.getName)

  test("uniquely solved: a portfolio compared with the tools not its members") {
    try {
      Settings.portfoliosFromArgs += VirtualPortfolio("P", Set("a", "b"), Set("all"))
      Settings.incorrectAsUnsolved = true
      val group = Main.readGroup("g1", ymlFiles)
      val members = Map("V. Portfolio [P]" -> Set("a", "b"))
      def unique(tool : String) = {
        val runs = GroupTables.uniquelySolved(
          group, group.toolRuns.find(_._1.fullToolName == tool).get._1, members).get
        (runs.correctSatRuns.length, runs.correctUnsatRuns.length)
      }
      assert(unique("a") == (0, 1))
      assert(unique("b") == (1, 1))
      assert(unique("c") == (0, 0))
      assert(unique("V. Portfolio [P]") == (2, 2))
    } finally {
      Settings.incorrectAsUnsolved = false
      Settings.portfoliosFromArgs.clear()
    }
  }

  private def run(name : String, result : Result, duration : Double,
                  peakMemoryKB : Option[Long]) =
    RunInfo(name, Unknown, result, duration, peakMemoryKB = peakMemoryKB)

  private val runs = RunInfos(Seq(
    run("p1", True, 1.0, Some(100)), run("p2", False, 2.0, Some(300)),
    run("p3", True, 280.0, None), run("p4", Timeout, 300.0, Some(900))))

  test("wall times: percentiles of the solved benchmarks by the nearest rank, late ones") {
    assert(GroupTables.wallTimes(runs, 270.0) ==
             GroupTables.WallTimes(3, Some(2.0), Some(280.0), Some(280.0),
                                   Some(280.0), 1))
  }

  test("peak memory: solved benchmarks and all runs, runs without it left out") {
    assert(GroupTables.peakMemory(runs) ==
             GroupTables.PeakMemory(Some(100.0), Some(300.0),
                                    Some(300.0), Some(900.0)))
  }
}
