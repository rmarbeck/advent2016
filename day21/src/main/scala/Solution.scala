object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (part1Initial, part2Initial) = inputLines.length match
      case 8 => ("abcde", "decab")
      case _ => ("abcdefgh", "fbgdceah")


    val actions = inputLines.collect:
      case s"swap position $x with position $y" => SwapPosition(x.toInt, y.toInt)
      case s"swap letter $x with letter $y" => SwapLetter(x.head, y.head)
      case s"rotate $direction $x step$_" =>
        direction match
          case "left" => RotateLeft(x.toInt)
          case _ => RotateRight(x.toInt)
      case s"rotate based on position of letter $x" => RotateOnPosition(x.head)
      case s"reverse positions $x through $y" => ReversePositions(x.toInt, y.toInt)
      case s"move position $x to position $y" => MovePosition(x.toInt, y.toInt)

    val resultPart1 = actions.foldLeft(part1Initial):
      case (acc, action: Action) => action.forward(acc)

    val resultPart2 = actions.reverse.foldLeft(part2Initial):
      case (acc, action: Action) => action.backward(acc)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

trait Action:
  def forward(input: String): String
  def backward(input: String): String

case class SwapPosition(xPos: Int, yPos: Int) extends Action:
  override def forward(input: String): String =
    input.zipWithIndex.map:
      case (_, index) if index == xPos => input(yPos)
      case (_, index) if index == yPos => input(xPos)
      case (letter, _) => letter
    .mkString

  override def backward(input: String): String = SwapPosition(yPos, xPos).forward(input)

case class SwapLetter(letterX: Char, letterY: Char) extends Action:
  override def forward(input: String): String =
    val xIndexes = input.zipWithIndex.filter(_._1 == letterX).map(_._2)
    val yIndexes = input.zipWithIndex.filter(_._1 == letterY).map(_._2)
    input.zipWithIndex.map:
      case (_, index) if xIndexes.contains(index) => letterY
      case (_, index) if yIndexes.contains(index) => letterX
      case (letter, _) => letter
    .mkString

  override def backward(input: String): String = SwapLetter(letterY, letterX).forward(input)

case class RotateLeft(steps: Int) extends Action:
  override def forward(input: String): String =
    require(input.length >= steps)
    s"${input.drop(steps)}${input.take(steps)}"

  override def backward(input: String): String = RotateRight(steps).forward(input)

case class RotateRight(steps: Int) extends Action:
  override def forward(input: String): String =
    require(input.length >= steps)
    s"${input.takeRight(steps)}${input.dropRight(steps)}"

  override def backward(input: String): String = RotateLeft(steps).forward(input)

case class RotateOnPosition(letter: Char) extends Action:
  private def computeNext(index: Int, size: Int): (Int, Int) =
    val nbMove = index match
      case value if value >= 4 => value + 2
      case value => value + 1
    val newIndex = (index + nbMove) % size
    (nbMove, newIndex)

  override def forward(input: String): String =
    def rotateRightNTime(n: Int): String =
      (1 to n).foldLeft(input):
        (acc, _) => RotateRight(1).forward(acc)

    val (nbMove, _) = computeNext(input.indexOf(letter), input.length)
    rotateRightNTime(nbMove)

  import scala.collection.mutable.Map
  private lazy val cachedMoves: Map[Int, Int] = Map[Int, Int]()

  override def backward(input: String): String =
    def computeNbMove(currentPosition: Int): Int =
      cachedMoves.contains(currentPosition) match
        case true => cachedMoves(currentPosition)
        case false =>
          (0 until input.length).map:
            computeNext(_, input.length)
          .foreach:
            (nbMove, newIndex) => cachedMoves.update(newIndex, nbMove)
          cachedMoves(currentPosition)

    def rotateRightBackWardtNTime(n: Int): String =
      (1 to n).foldLeft(input):
        (acc, _) => RotateRight(1).backward(acc)

    rotateRightBackWardtNTime(computeNbMove(input.indexOf(letter)))


case class ReversePositions(xPos: Int, yPos: Int) extends Action:
  override def forward(input: String): String =
    s"${input.take(xPos)}${input.slice(xPos, yPos+1).reverse}${input.drop(yPos+1)}"

  override def backward(input: String): String = forward(input)

case class MovePosition(xPos: Int, yPos: Int) extends Action:
  override def forward(input: String): String =
    xPos < yPos match
      case true => s"${input.take(xPos)}${input.slice(xPos+1, yPos+1)}${input(xPos)}${input.drop(yPos+1)}"
      case false => s"${input.take(yPos)}${input(xPos)}${input.slice(yPos, xPos)}${input.drop(xPos+1)}"

  override def backward(input: String): String = MovePosition(yPos, xPos).forward(input)