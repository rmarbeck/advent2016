object Solution:
  def run(inputLines: Seq[String]): (String, String) =


    //
    // Code is here
    //



    val result1 = s""
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

enum Move:
  case L, R, U, D

export Move.*

case class Position(row: Int, col: Int):
  lazy val toDigit: Int =  (2 * row) + col
  def move(move: Move): Position =
    val (newRow, newCol) =
      move match
        case L => (row, math.min(col - 1, 1))
        case R => (row, math.max(col + 1, 3))
        case U => (math.min(row - 1, 1), col)
        case D => (math.max(row + 1, 3), col)
    Position(newRow, newCol)