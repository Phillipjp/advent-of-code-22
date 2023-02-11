package adventofcode2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day5Spec extends AnyWordSpec with Matchers {

  "makeInstructions" should {
    "make instructions" in {
      val input = List(
      "move 1 from 2 to 1",
      "move 3 from 1 to 3",
      "move 2 from 2 to 1",
      "move 1 from 1 to 2"
      )

      Day5.makeInstructions(input) shouldBe List(
        Instruction(1,1,0),
        Instruction(3,0,2),
        Instruction(2,1,0),
        Instruction(1,0,1)
      )

    }
  }

  "moveCrates" should{

    "move the crates according to the instructions" in {
      val instructions = List(
        Instruction(1,1,0),
        Instruction(3,0,2),
        Instruction(2,1,0),
        Instruction(1,0,1)
      )

      val crates = List(
        List("N", "Z"),
        List("D", "C", "M"),
        List("P")
      )

      val getNewDestinationStack: (List[List[String]], Instruction) => List[String] = (c, instruction) => c(instruction.source).take(instruction.amount).reverse ++ c(instruction.destination)

      Day5.moveCrates(crates, instructions, getNewDestinationStack) shouldBe List(
        List("C"),
        List("M"),
        List("Z","N","D","P")
      )
    }
  }

}
