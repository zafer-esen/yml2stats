package yml2stats

import org.scalatest._

class HelloSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Main shouldEqual "hello"
  }
}
