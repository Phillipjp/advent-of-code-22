package adventofcode2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Spec extends AnyWordSpec with Matchers{

  "splitCompartments" should{
    "split a string into a bag with equal compartments" in {
      val items = "vJrwpWtwJgWrhcsFMMfFFhFp"
      Day3.makeBag(items) shouldBe Bag(List('v','J','r','w','p','W','t','w','J','g','W','r'), List('h','c','s','F','M','M','f','F','F','h','F','p'))
    }
  }

  "getCompartmentCrossovers" should {
    "only return the items that are in both compartments" in {
      val bag = Bag(List('v','J','r','w','p','W','t','w','J','g','W','r'), List('h','c','s','F','M','M','f','F','F','h','F','p'))
      Day3.getCrossovers(bag.compartment1, bag.compartment2) shouldBe List('p')
    }
  }

  "partOne" should {
    "get the sum of the priorities" in {
      val input = List(
        "vJrwpWtwJgWrhcsFMMfFFhFp",
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
        "PmmdzqPrVvPwwTWBwg",
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
        "ttgJtRGJQctTZtZT",
        "CrZsJsPPZsGzwwsLwLmpwMDw"
      )
      Day3.partOne(input, Day3.itemPriorities()) shouldBe 157

    }
  }

  "findBadgeForGroup" should {
    "find the item in all 3 bags" in {
      Day3.findBadgeForGroup(
        "vJrwpWtwJgWrhcsFMMfFFhFp",
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
        "PmmdzqPrVvPwwTWBwg"
      ).toString shouldBe "r"
    }
  }

  "partTwo" should {
    "get the sum of the priorities" in {
      val input = List(
        "vJrwpWtwJgWrhcsFMMfFFhFp",
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
        "PmmdzqPrVvPwwTWBwg",
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
        "ttgJtRGJQctTZtZT",
        "CrZsJsPPZsGzwwsLwLmpwMDw"
      )
      Day3.partTwo(input, Day3.itemPriorities()) shouldBe 70

    }
  }

}
