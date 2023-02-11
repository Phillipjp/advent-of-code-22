package adventofcode2022

object Day4 extends App {



  def makeElfPairs(input: List[String]): List[ElfPair] = {
    def makeElf(s:String): List[Int] = {
      val (start, end) = s.split("-").toList match {
        case s :: e :: Nil => (s.toInt, e.toInt)
      }
      (start to end).toList
    }

    def makeElfPair(s: String): ElfPair = {
      s.split(",").toList match {
        case e1 :: e2 :: Nil => ElfPair(makeElf(e1), makeElf(e2))
      }
    }

    input.map(makeElfPair)
  }

  def isCompleteOverlap(elfPair: ElfPair): Boolean = {
    elfPair.a.forall(elfPair.b.contains) || elfPair.b.forall(elfPair.a.contains)
  }

  def isAtLeastOneOverlap(elfPair: ElfPair): Boolean = {
    elfPair.a.exists(elfPair.b.contains)
  }

  def findNUmberOfOverlaps(input: List[String], isOverlap: ElfPair => Boolean): Int = {
    makeElfPairs(input).count(isOverlap)
  }

  val input = Utils.readFileAsListOfString("day-4.txt")
  val partOneAnswer = findNUmberOfOverlaps(input, isCompleteOverlap)
  println(partOneAnswer)
  val partTwoAnswer = findNUmberOfOverlaps(input, isAtLeastOneOverlap)
  println(partTwoAnswer)

}

case class ElfPair(a: List[Int], b: List[Int])
