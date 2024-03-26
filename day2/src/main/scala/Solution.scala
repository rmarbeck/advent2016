object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val positions = inputLines.mkString("|").foldLeft(Position.start):
      case (current @ head :: tail, moveOrSep) =>
        moveOrSep match
          case Move(move) => Position.move(head, move) :: tail
          case _ => head +: current
      case _ => throw Exception("Not managed")

    val (resultPart1, resultPart2) = positions.reverse.map(Position.toDigit).unzip

    val result1 = s"${resultPart1.mkString}"
    val result2 = s"${resultPart2.mkString}"

    (s"${result1}", s"${result2}")

end Solution

enum Move:
  case L, R, U, D

object Move:
  private val orderedValues = "LRUD"
  def unapply(char: Char): Option[Move] =
    orderedValues.indexOf(char) match
      case -1 => None
      case index => Some(Move.fromOrdinal(index))

export Move.*

type Part1AndPart2Positions = (Position, Position)

case class Position(row: Int, col: Int):
  private val part2Digits = "123456789ABCD"
  private lazy val toDigitPart1: Char =  s"${(3 * (row - 1)) + col}".head
  private lazy val toDigitPart2: Char =
    val index = (row - 3, col - 3) match
      case (-2, _) => 1
      case (2, _) => 13
      case (rowDiff, colDiff) => (7 + 4*rowDiff) + colDiff

    part2Digits(index-1)

  private def movePart1(move: Move): Position =
    val (newRow, newCol) =
      move match
        case L => (row, math.max(col - 1, 1))
        case R => (row, math.min(col + 1, 3))
        case U => (math.max(row - 1, 1), col)
        case D => (math.min(row + 1, 3), col)
    Position(newRow, newCol)

  private def movePart2(move: Move): Position =
    def inStdKeyPad(value: Int): Boolean = value >=2 && value <=4
    val (newRow, newCol) =
      (row, col, move) match
        case (2, 3, U) => (1, 3)
        case (3, 2, L) => (3, 1)
        case (3, 4, R) => (3, 5)
        case (4, 3, D) => (5, 3)
        case (curRow, curCol, direction) if inStdKeyPad(curRow) && inStdKeyPad(curCol) =>
          val Position(calcRow, calcCol) = Position(curRow - 1, curCol - 1).movePart1(direction)
          (calcRow + 1, calcCol + 1)
        case (1, 3, D) => (2, 3)
        case (5, 3, U) => (4, 3)
        case (3, 1, R) => (3, 2)
        case (3, 5, L) => (3, 4)
        case _ => (row, col)

    Position(newRow, newCol)

object Position:
  val start: List[Part1AndPart2Positions] = List((Position(2, 2), Position(3, 1)))
  def move(positions: Part1AndPart2Positions, move: Move): Part1AndPart2Positions =
    (positions._1.movePart1(move), positions._2.movePart2(move))

  def toDigit(positions: Part1AndPart2Positions): (Char, Char) =
    (positions._1.toDigitPart1, positions._2.toDigitPart2)