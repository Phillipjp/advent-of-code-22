package adventofcode2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day1Spec extends AnyWordSpec with Matchers {

  "partOne" should {
    "return the largest sum set of calories" in {
      val calories = List("1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "", "10000")
      Day1.partOne(calories) shouldBe Right(24000)
    }

    "return None if unable to process list" in {
      val calories = List("1000", "2000", "3000", "", "blah")
      Day1.partOne(calories) shouldBe Left("Unable to parse 'blah' to an Int'")
    }
  }

  "partTwo" should {
    "return the sum of the 3 largest sets of calories" in {
      val calories = List("1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "", "10000")
      Day1.partTwo(calories) shouldBe Right(45000)
    }
  }
}
