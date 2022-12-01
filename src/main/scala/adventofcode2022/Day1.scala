package adventofcode2022

object Day1 extends App{

  private def sumCalories(calories: Seq[String]): Either[String, List[Int]] = {
    calories.appended("").foldLeft(Right((List.empty[Int], 0)): Either[String, (List[Int], Int)]){
      case (Right((sums, currentSum)), "") =>  Right((sums :+ currentSum, 0))
      case (Right((sums, currentSum)), s) => Either.cond(s.toIntOption.isDefined, (sums, currentSum + s.toIntOption.get), s"Unable to parse '$s' to an Int'")
      case (left, _) => left
    }.map(_._1)
  }

  def partOne(calories: Seq[String]): Either[String, Int] = {
      sumCalories(calories).map(_.max)
  }

  def partTwo(calories: Seq[String]): Either[String, Int] = {
    sumCalories(calories).map(_.sorted.takeRight(3).sum)
  }

  val input: Seq[String] = Utils.readFileAsListOfString("day-1.txt")

  println(s" Day 1 Part 1: ${partOne(input)}")
  println(s" Day 2 Part 2: ${partTwo(input)}")


}
