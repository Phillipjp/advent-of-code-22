package adventofcode2022

object Day2 extends App {

  private def scoreRound(hs1: HandShape, hs2:HandShape): Int = {
     hs1.compareHandShape(hs2) +  hs1.value
  }

  private def makeStrategyGuidePartOne(input: List[String]): List[(HandShape, HandShape)] = {
    input.map{line =>charToHandShape(line.head) -> charToHandShape(line.last)}
  }

  private def makeStrategyGuidePartTwo(input: List[String]): List[(HandShape, Result)] = {
    input.map{line =>charToHandShape(line.head) -> charToResult(line.last)}
  }

  private def charToHandShape(c: Char): HandShape = {
    c match {
      case 'A' | 'X' => Rock
      case 'B' | 'Y' => Paper
      case 'C' | 'Z' => Scissors
    }
  }

  private def charToResult(c: Char): Result = {
    c match {
      case 'X' => Lose
      case 'Y' => Draw
      case 'Z' => Win
    }
  }

  private def pickMove(opponentHandShape: HandShape, result: Result): HandShape =
    result match{
      case Win => opponentHandShape.inferiorTo
      case Lose => opponentHandShape.superiorTo
      case Draw => opponentHandShape

  }

  def partOne(input: List[String]): Int ={
    val sg = makeStrategyGuidePartOne(input)
    sg.map{case (opponentsHandShape, handShape) => scoreRound(handShape, opponentsHandShape)}.sum
  }

  def partTwo(input: List[String]): Int ={
    val sg = makeStrategyGuidePartTwo(input)
    sg.map{case (opponentHandShape, result) =>
      val handShape = pickMove(opponentHandShape, result)
      scoreRound(handShape, opponentHandShape)
    }.sum
  }




  val input = Utils.readFileAsListOfString("day-2.txt")
  println(partOne(input))
  println(partTwo(input))


}

sealed trait HandShape{
  val value: Int
  val superiorTo: HandShape
  val inferiorTo: HandShape
  def compareHandShape(other: HandShape): Int

}

case object Rock extends HandShape {
  val value = 1
  override val superiorTo: HandShape = Scissors
  override val inferiorTo: HandShape = Paper
  def compareHandShape(other: HandShape): Int = {
    other match {
      case Rock => 3
      case Paper => 0
      case Scissors => 6
    }
  }
}
case object Paper extends HandShape {
  val value = 2
  override val superiorTo: HandShape = Rock
  override val inferiorTo: HandShape = Scissors
  def compareHandShape(other: HandShape): Int = {
    other match {
      case Rock => 6
      case Paper => 3
      case Scissors => 0
    }
  }
}
case object Scissors extends HandShape {
  val value = 3
  override val superiorTo: HandShape = Paper
  override val inferiorTo: HandShape = Rock
  def compareHandShape(other: HandShape): Int = {
    other match {
      case Rock => 0
      case Paper => 6
      case Scissors => 3
    }
  }
}

sealed trait Result

case object Win extends Result
case object Lose extends Result
case object Draw extends Result
