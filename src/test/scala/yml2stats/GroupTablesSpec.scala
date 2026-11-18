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
}
