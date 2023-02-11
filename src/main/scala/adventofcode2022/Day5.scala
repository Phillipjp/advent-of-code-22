package adventofcode2022


/*

[T]     [D]         [L]
[R]     [S] [G]     [P]         [H]
[G]     [H] [W]     [R] [L]     [P]
[W]     [G] [F] [H] [S] [M]     [L]
[Q]     [V] [B] [J] [H] [N] [R] [N]
[M] [R] [R] [P] [M] [T] [H] [Q] [C]
[F] [F] [Z] [H] [S] [Z] [T] [D] [S]
[P] [H] [P] [Q] [P] [M] [P] [F] [D]
 1   2   3   4   5   6   7   8   9


 */
object Day5 extends App{

  val crates = List(
    List("T","R","G","W","Q","M","F","P"),
    List("R","F","H"),
    List("D","S","H","G","V","R","Z","P"),
    List("G","W","F","B","P","H","Q"),
    List("H","J","M","S","P"),
    List("L","P","R","S","H","T","Z","M"),
    List("L","M","N","H","T","P"),
    List("R","Q","D","F"),
    List("H","P","L","N","C","S","D")
  )

  def makeInstructions(input: List[String]): List[Instruction] = {
    def makeInstruction(s: String): Instruction = {
      val split = s.split(" ")
      Instruction(split(1).toInt, split(3).toInt - 1, split.last.toInt - 1)
    }
    input.map(makeInstruction)
  }

  def moveCrates(crates: List[List[String]], instructions: List[Instruction],
                 getNewDestinationStack: (List[List[String]], Instruction) => List[String]): List[List[String]] = {
    instructions.foldLeft(crates){case (c,instruction) =>
    val newSourceStack = c(instruction.source).drop(instruction.amount)
    val newDestinationStack = getNewDestinationStack(c, instruction)
    c
      .zipWithIndex
      .map{case (stack, i) =>
      if(i == instruction.source)
        newSourceStack
      else if(i == instruction.destination)
        newDestinationStack
      else
        stack
      }
    }
  }

  def getTopCrates(crates: List[List[String]]): List[String] = {
    crates.map(_.head)
  }

  def partOne(crates: List[List[String]], instructions: List[Instruction]): String = {
    val getNewDestinationStack: (List[List[String]], Instruction) => List[String] = (c, instruction) => c(instruction.source).take(instruction.amount).reverse ++ c(instruction.destination)
    val movedCrates = moveCrates(crates, instructions, getNewDestinationStack)
    val topCrates = getTopCrates(movedCrates)
    topCrates.mkString
  }

  def partTwo(crates: List[List[String]], instructions: List[Instruction]): String = {
    val getNewDestinationStack: (List[List[String]], Instruction) => List[String] = (c, instruction) => c(instruction.source).take(instruction.amount) ++ c(instruction.destination)
    val movedCrates = moveCrates(crates, instructions, getNewDestinationStack)
    val topCrates = getTopCrates(movedCrates)
    topCrates.mkString
  }


  val input = Utils.readFileAsListOfString("day-5.txt")
  val instructions = makeInstructions(input)

  val partOneAnswer = partOne(crates, instructions)
  println(partOneAnswer)
  val partTwoAnswer = partTwo(crates, instructions)
  println(partTwoAnswer)


}

case class Instruction(amount: Int, source: Int, destination: Int)
