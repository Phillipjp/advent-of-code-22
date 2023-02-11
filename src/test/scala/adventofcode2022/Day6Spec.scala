package adventofcode2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Spec extends AnyWordSpec with Matchers {

  "findSomIndex" should {
    "get the correct index" in {
      Day6.findSomIndex("bvwbjplbgvbhsrlpgdmjqwftvncz", 4) shouldBe 5
      Day6.findSomIndex("nppdvjthqldpwncqszvftbrmjlhg", 4) shouldBe 6
      Day6.findSomIndex("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4) shouldBe 10
      Day6.findSomIndex("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4) shouldBe 11
    }
  }

}
