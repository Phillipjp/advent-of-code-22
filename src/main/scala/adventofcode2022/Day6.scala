package adventofcode2022

import scala.annotation.tailrec

object Day6 extends App{



  def findSomIndex(input: String, size: Int): Int = {

    @tailrec
    def go(signal: List[String], pos: Int): Int = {
      if(signal.head.distinct.length == size)
        pos + size
      else
        go(signal.tail, pos + 1)
    }

    go(input.sliding(size).toList, 0)
  }

  val input = Utils.readFileAsListOfString("day-6.txt").head

  val partOneAnswer  = findSomIndex(input, 4)
  println(partOneAnswer)
  val partTwoAnswer  = findSomIndex(input, 14)
  println(partTwoAnswer)

}
