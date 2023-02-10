package adventofcode2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day2Spec extends AnyWordSpec with Matchers{

  "partOne" should {
    "sum the score following the strategy" in {
      val input = List("A Y", "B X", "C Z")
      Day2.partOne(input) shouldBe 15

    }
  }

  "partTwo" should {
    "sum the score following the strategy" in {
      val input = List("A Y", "B X", "C Z")
      Day2.partTwo(input) shouldBe 12

    }
  }
}
