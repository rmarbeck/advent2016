import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val nbInput = inputLines.head.toInt


    val result1 = s"${findLastElf(nbInput)}"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

def findLastElf(numberOfElves: Int): Int =
  def lastPowerOfTwoBelow: Int =
    def loop(current: Int): Int =
      math.pow(2, current) > numberOfElves match
        case true => math.pow(2, current - 1).toInt
        case false => loop(current + 1)
    loop(0)
  2 * (numberOfElves - lastPowerOfTwoBelow) + 1

