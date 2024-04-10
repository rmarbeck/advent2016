import scala.annotation.tailrec

val startingPosition: Position = Position(1, 1)
val goal: Position = Position(4, 4)

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val passcode = inputLines.head
    val start = List((passcode, startingPosition))

    val resultPart1 = shortestPath(start, passcode)
    val resultPart2 = longestPathLength(start, passcode)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

type Path = (String, Position)
type Move = (Char, Position)

@tailrec
def shortestPath(current: List[Path], passcode: String): String =
    current match
      case Nil => throw Exception("Not found")
      case head :: tail =>
        val (currentPathString, currentPosition) = head
        currentPosition match
          case pos if pos == goal => currentPathString drop passcode.length
          case pos =>
            val next = pos.nextComputer(currentPathString).map:
              (newDir, newPosition) => (s"$currentPathString$newDir", newPosition)
            val newCurrentPositionsSorted = (tail ::: next).sortBy(_._1.length)
            shortestPath(newCurrentPositionsSorted, passcode)


@tailrec
def longestPathLength(current: List[Path], passcode: String, maximum: Int = 0): Int =
  current match
    case Nil => maximum - passcode.length
    case head :: tail =>
      val (currentPathString, currentPosition) = head
      currentPosition match
        case pos if pos == goal => longestPathLength(tail, passcode, math.max(maximum, currentPathString.length))
        case pos =>
          val next = pos.nextComputer(currentPathString).map:
            (newDir, newPosition) => (s"${currentPathString}$newDir", newPosition)
          val newCurrentPositions = tail ::: next
          longestPathLength(newCurrentPositions, passcode, maximum)

case class Position(row: Int, col: Int):
  private lazy val up = this.copy(row = row - 1)
  private lazy val down = this.copy(row = row + 1)
  private lazy val left = this.copy(col = col - 1)
  private lazy val right = this.copy(col = col + 1)

  private def isValid: Boolean = row >=1 && row <=4 && col >=1 && col <=4

  def nextComputer(from: String): List[Move] =
    next(MD5.hash(s"${from}").take(4))

  private def next(md5Value : String): List[Move] =
    md5Value.toList.map:
      case 'b' | 'c' | 'd' | 'e' | 'f' => true
      case _ => false
    .zip(List('U', 'D', 'L', 'R')).collect:
      case (true, dir) => Some(dir)
      case (false, _) => None
    .zip(List(up, down, left, right)).collect:
      case (Some(dir), position) if position.isValid => (dir, position)

object MD5:
  def hash(s: String): String =
    val m = java.security.MessageDigest.getInstance("MD5")
    val b = s.getBytes("UTF-8")
    m.update(b, 0, b.length)
    val result = new java.math.BigInteger(1, m.digest()).toString(16)
    32 - result.length match
      case 0 => result
      case value => "0" * value + result
