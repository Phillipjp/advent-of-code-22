package adventofcode2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day4Spec extends AnyWordSpec with Matchers {

  "makeElfPairs" should {

    "make a list of elf pairs" in {
      val input = List(
      "2-4,6-8",
      "2-3,4-5",
      "5-7,7-9",
      "2-8,3-7",
      "6-6,4-6",
      "2-6,4-8"
      )

      Day4.makeElfPairs(input) shouldBe List(
        ElfPair(List(2,3,4), List(6,7,8)),
        ElfPair(List(2,3), List(4,5)),
        ElfPair(List(5,6,7), List(7,8,9)),
        ElfPair(List(2,3,4,5,6,7,8), List(3,4,5,6,7)),
        ElfPair(List(6), List(4,5,6)),
        ElfPair(List(2,3,4,5,6), List(4,5,6,7,8)),
      )
    }

  }

  "isCompleteOverlap" should {
    "return true if there is a complete overlap" in {
      Day4.isCompleteOverlap(ElfPair(List(2,3,4,5,6,7,8), List(3,4,5,6,7))) shouldBe true
      Day4.isCompleteOverlap(ElfPair(List(6), List(4,5,6))) shouldBe true
    }

    "return false if there is not a complete overlap" in {
      Day4.isCompleteOverlap(ElfPair(List(2,3,4), List(6,7,8))) shouldBe false
      Day4.isCompleteOverlap(ElfPair(List(2,3), List(4,5))) shouldBe false
      Day4.isCompleteOverlap(ElfPair(List(5,6,7), List(7,8,9))) shouldBe false
      Day4.isCompleteOverlap(ElfPair(List(2,3,4,5,6), List(4,5,6,7,8))) shouldBe false
    }
  }



}
