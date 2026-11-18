package yml2stats

import org.scalatest.FunSuite
import yml2stats.Benchmarks._

// the group g1: tools a, b, c on p1 to p5; a answers sat on p3, expected unsat
class StatusRulesSpec extends FunSuite {
  private val ymlFiles =
    new java.io.File("src/test/resources/g1").listFiles.toList.sortBy(_.getName)

  private def runsOf(group : Group, tool : String) : RunInfos =
    group.toolRuns.find(_._1.fullToolName == tool).get._2

  test("expected verdicts from -expected replace those of the logs") {
    try {
      Settings.expectedVerdicts("dir/p3.smt2") = True
      val group = Main.readGroup("g1", ymlFiles)
      assert(runsOf(group, "a").incorrectRuns.isEmpty)
      assert(runsOf(group, "a").correctSatRuns.length == 3)
      assert(runsOf(group, "b").incorrectRuns.map(_.bmName) == Seq("dir/p3.smt2"))
    } finally Settings.expectedVerdicts.clear()
  }

  test("with -incorrect-as-unsolved a portfolio solves only with correct verdicts") {
    try {
      Settings.portfoliosFromArgs += VirtualPortfolio("P", Set("a", "b"), Set("all"))
      val faster = runsOf(Main.readGroup("g1", ymlFiles), "V. Portfolio [P]")
      assert(faster.incorrectRuns.map(_.bmName) == Seq("dir/p3.smt2"))
      Settings.incorrectAsUnsolved = true
      val portfolio = runsOf(Main.readGroup("g1", ymlFiles), "V. Portfolio [P]")
      assert(portfolio.incorrectRuns.isEmpty)
      assert(portfolio.correctUnsatRuns.map(_.bmName) ==
               Seq("dir/p2.smt2", "dir/p3.smt2"))
    } finally {
      Settings.incorrectAsUnsolved = false
      Settings.portfoliosFromArgs.clear()
    }
  }
}
