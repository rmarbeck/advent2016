import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val nbInput = inputLines.head.toInt

    val result1 = s"${findLastElf(nbInput)}"
    val result2 = s"${findLastBuffered(ArrayBuffer.tabulate(nbInput)(_ + 1), nbInput)}"

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


@tailrec
def findLast(elves: Array[Int], index: Int = 0): Int =
  println(elves.size)
  elves.size match
    case 1 => elves(0)
    case size =>
      val indexOfElfToRemove = (index + (size / 2)) % size
      elves(indexOfElfToRemove) = 0
      findLast(elves.filterNot(_ == 0), (index + 1) % size)


@tailrec
def findLastBuffered(elves: ArrayBuffer[Int], size: Int, index: Int = 0, start: Long = System.currentTimeMillis()): Int =
  size match
    case 1 => elves(0)
    case size =>
      val newStart = size % 10000 == 0 match
        case true =>
          val now = System.currentTimeMillis()
          println(s"${now - start}ms (for size : $size)")
          now
        case false => start

      val indexOfElfToRemove = (index + (size / 2)) % size
      elves.remove(indexOfElfToRemove)
      val newSize = size - 1
      if (indexOfElfToRemove < (index % size))
        findLastBuffered(elves, newSize, index % newSize, newStart)
      else
        findLastBuffered(elves, newSize, (index + 1) % newSize, newStart)

