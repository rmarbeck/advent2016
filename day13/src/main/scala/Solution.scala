import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val favoriteNumber = inputLines.head.toInt
    val fromPosition = Position(1, 1)
    val destination = favoriteNumber match
      case 10 => Position(7,4)
      case _ => Position(31, 39)

    val resultPart1 = findShortest(fromPosition, destination, favoriteNumber)
    val resultPart2 = differentPositions(fromPosition, 50, favoriteNumber)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Position(row: Int, col: Int):
  private lazy val up = this.copy(row = row - 1)
  private lazy val down = this.copy(row = row + 1)
  private lazy val right = this.copy(col = col + 1)
  private lazy val left = this.copy(col = col - 1)

  private def isValid: Boolean = row >= 0 && col >= 0

  private def canWalk(number: Int): Boolean =
    val (x, y) = (row, col)
    val firstStep: Int = (x * x) + (3 * x) + 2 * (x * y) + y + (y * y)
    val secondStep: Int = firstStep + number
    secondStep.toBinaryString.count(_ == '1') % 2 == 0

  def nextWithNumber(number:Int): Seq[Position] =
    val result = Seq(up, down, right, left).filter(_.isValid).filter(_.canWalk(number))
    result

case class PositionWithDistance(position: Position, distance: Int)
case class PositionWithSteps(position: Position, steps: Int)

@tailrec
def findShortest(from: Vector[PositionWithDistance], to: Position, current: Int, withNumber: Int, explored: Set[Position] = Set()): Int =
  val (head, tail) = (from.head, from.tail)
  head.position == to match
    case true => head.distance
    case false if explored.contains(head.position) => findShortest(tail, to, current, withNumber, explored)
    case false =>
      findShortest(tail :++ head.position.nextWithNumber(withNumber).map(pos => PositionWithDistance(pos, head.distance + 1)), to, current + 1, withNumber, explored + head.position)


def findShortest(from: Position, to: Position, favoriteNumber: Int): Int =
  findShortest(Vector(PositionWithDistance(from, 0)), to, 0, favoriteNumber)

@tailrec
def differentPositions(from: Vector[PositionWithSteps], maxSteps: Int, withNumber: Int, visited: Set[Position] = Set()): Int =
  val (head, tail) = (from.head, from.tail)
  head.steps > maxSteps match
    case true => visited.size
    case false if visited.contains(head.position) => differentPositions(tail, maxSteps, withNumber, visited)
    case false =>
      differentPositions(tail :++ head.position.nextWithNumber(withNumber).map(pos => PositionWithSteps(pos, head.steps + 1)), maxSteps, withNumber, visited + head.position)

def differentPositions(from: Position, maxSteps: Int, favoriteNumber: Int): Int =
  differentPositions(Vector(PositionWithSteps(from, 0)), maxSteps, favoriteNumber)

