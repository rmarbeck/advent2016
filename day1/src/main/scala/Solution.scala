import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val InstructionMapper = """(R|L)(\d+)""".r.unanchored

    val instructions = InstructionMapper.findAllIn(inputLines.head).toArray.collect:
      case InstructionMapper(rotation, steps) => Instruction(rotation, steps.toInt)

    val initialPosition = Position(0, 0, N)
    val allStops = instructions.scanLeft(initialPosition):
      case (currentPosition @ Position(_, _, direction), Instruction(rotation, steps)) =>
        val newDirection =  direction.rotate(rotation)
        currentPosition.walk(steps, newDirection).copy(direction = newDirection)

    val resultPart1 = initialPosition.cabDistance(allStops.last)

    val it = allStops.sliding(2, 1).flatMap:
      case Array(start, end) => start.until(end)

    val resultPart2 = initialPosition.cabDistance(findFirst(it).get)


    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def findFirst(positions: Iterator[Position], loaded: List[Position] = Nil): Option[Position] =
  positions.nextOption() match
    case None => None
    case Some(value) =>
      loaded.exists(_ superpose value) match
        case true => Some(value)
        case false => findFirst(positions, value +: loaded)


enum Dir(val degrees: Int):
  def rotate(rotation: Rotation): Dir =
    def rotate(finalDegrees: Int): Dir =
      finalDegrees match
        case N.degrees => N
        case E.degrees => E
        case S.degrees => S
        case W.degrees => W
    (this.degrees + rotation.degrees) % 360 match
      case value if value < 0 => rotate(value + 360)
      case value => rotate(value)

  case N extends Dir(0)
  case E extends Dir(90)
  case S extends Dir(180)
  case W extends Dir(270)

enum Rotation(val degrees: Int):
  case R extends Rotation(90)
  case L extends Rotation(-90)

object Rotation:
  def from(char: Char) =
    char match
      case 'R' => R
      case 'L' => L

export Dir.*
export Rotation.*

type Copier = Int => Position

case class Position(row: Int, col: Int, direction: Dir):
  def superpose(other: Position): Boolean = row == other.row && col == other.col
  def until(other: Position): Iterable[Position] =
    def guessRangeAndCopier: (Range, Copier) =
      (other.row - row, other.col - col) match
        case (0, value) => (col until other.col by value.sign, newVal => this.copy(col = newVal))
        case (value, 0) => (row until other.row by value.sign, newVal => this.copy(row = newVal))
    val (range, copier) = guessRangeAndCopier

    range.map(copier)

  def cabDistance(other: Position): Int =
    (row - other.row).abs + (col - other.col).abs
  def walk(steps: Int, direction: Dir): Position =
    direction match
      case N => this.copy(row = row - steps)
      case S => this.copy(row = row + steps)
      case E => this.copy(col = col + steps)
      case W => this.copy(col = col - steps)

case class Instruction(rotation: Rotation, steps: Int)

object Instruction:
  def apply(rotationAsString: String, steps: Int): Instruction =
    Instruction(Rotation.from(rotationAsString.head), steps)