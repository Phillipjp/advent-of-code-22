package adventofcode2022

object Day3 extends App {



  def makeBag(items: String): Bag = {
    val compartments = items.toList.splitAt(items.size/2)
    Bag(compartments._1, compartments._2)
  }

  def getCrossovers(a: List[Char], b: List[Char]): List[Char] = {
    val distinctB = b.distinct
    a.distinct.filter(item => distinctB.contains(item))
  }

  def itemPriorities(): Map[Char, Int] = {
    (('a' to 'z').zipWithIndex.map{case(item, i) => (item, i+1)} ++ ('A' to 'Z').zipWithIndex.map{case(item, i) => (item, i+27)}).toMap
  }

  def partOne(input: List[String], priorities: Map[Char, Int]) : Int = {
    val bags = input.map(makeBag)
    val crossOvers = bags.flatMap(bag => getCrossovers(bag.compartment1, bag.compartment2))
    crossOvers.map(priorities(_)).sum
  }

  def findBadgeForGroup(elf1: String, elf2: String, elf3: String): Char = {
    val (bag1, bag2, bag3) = (makeBag(elf1), makeBag(elf2), makeBag(elf3))
    getCrossovers(getCrossovers(bag1.combinedCompartments, bag2.combinedCompartments), bag3.combinedCompartments).head
  }

  def partTwo(input: List[String], priorities: Map[Char, Int]): Int = {
    input
      .sliding(3, 3)
      .map{case (elf1 :: elf2 :: elf3 :: Nil) => findBadgeForGroup(elf1, elf2, elf3)}
      .map(priorities(_))
      .sum
  }

  val input = Utils.readFileAsListOfString("day-3.txt")
  val priorities = itemPriorities()
  val partOneAnswer = partOne(input, priorities)
  println(partOneAnswer)
  val partTwoAnswer = partTwo(input, priorities)
  println(partTwoAnswer)


}

case class Bag(compartment1: List[Char], compartment2: List[Char]){
  val combinedCompartments: List[Char] = compartment1 ++ compartment2
}
