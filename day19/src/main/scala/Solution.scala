import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val nbInput = inputLines.head.toInt

    val result1 = s"${findLastElfPart1(nbInput)}"
    val result2 = s"${findLastElfPart2(nbInput)}"

    (s"${result1}", s"${result2}")

end Solution

def findLastElfPart1(numberOfElves: Int): Int =
  def lastPowerOfTwoBelow: Int =
    @tailrec
    def loop(current: Int): Int =
      math.pow(2, current) > numberOfElves match
        case true => math.pow(2, current - 1).toInt
        case false => loop(current + 1)
    loop(0)
  2 * (numberOfElves - lastPowerOfTwoBelow) + 1



def findLastElfPart2(numberOfElves: Int): Int =
  def lastPowerOfThreeBelow: Int =
    @tailrec
    def loop(current: Int): Int =
      math.pow(3, current) > numberOfElves match
        case true => math.pow(3, current - 1).toInt
        case false => loop(current + 1)
    loop(0)
  numberOfElves - lastPowerOfThreeBelow
