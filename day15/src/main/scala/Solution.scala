import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val discsPart1 = inputLines.collect:
      case s"Disc #${id} has ${positions} positions; at time=0, it is at position ${initialPosition}." => Disc(id.toInt, positions.toInt, initialPosition.toInt)

    val resultPart1 = find(0, discsPart1)

    val discsPart2 = discsPart1 :+ Disc(discsPart1.length + 1, 11, 0)

    val resultPart2 = find(0, discsPart2)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def find(currentIndex: Int, discs: Seq[Disc]): Int =
  if (discs.forall(_.isOk(currentIndex)))
    currentIndex
  else
    find(currentIndex+1, discs)

case class Disc(id: Int, positions: Int, initialPosition: Int):
  def isOk(atTime: Int): Boolean = (initialPosition + id + atTime) % positions == 0