object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val initial = inputLines.length match
      case 8 => "abcde"
      case _ => "abcdefgh"


    val actions = inputLines.collect:
      case s"swap position $x with position $y" => SwapPosition(x.toInt, y.toInt)
      case s"swap letter $x with letter $y" => SwapLetter(x.head, y.head)
      case s"rotate left $x step$_" => RotateLeft(x.toInt)
      case s"rotate right $x step$_" => RotateRight(x.toInt)
      case s"rotate based on position of letter $x" => RotateOnPosition(x.head)
      case s"reverse positions $x through $y" => ReversePositions(x.toInt, y.toInt)
      case s"move position $x to position $y" => MovePosition(x.toInt, y.toInt)

    val resultPart1 = actions.foldLeft(initial):
      case (acc, action: Action) => action.forward(acc)

    val resultPart2 = actions.reverse.foldLeft("fbgdceah"):
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
    s"${input.drop(steps)}${input.take(steps)}"

  override def backward(input: String): String = RotateRight(steps).forward(input)

case class RotateRight(steps: Int) extends Action:
  override def forward(input: String): String =
    s"${input.takeRight(steps)}${input.dropRight(steps)}"

  override def backward(input: String): String = RotateLeft(steps).forward(input)

case class RotateOnPosition(letter: Char) extends Action:
  override def forward(input: String): String =
    def rotateRightNTime(n: Int): String =
      (1 to n).foldLeft(input):
        (acc, _) => RotateRight(1).forward(acc)
    val index = input.indexOf(letter)
    index match
      case value if value >= 4 => rotateRightNTime(index + 2)
      case _ => rotateRightNTime(index + 1)

  override def backward(input: String): String =
    def rotateLeftNTime(n: Int): String =
      (1 to n).foldLeft(input):
        (acc, _) => RotateLeft(1).forward(acc)

    val index = input.indexOf(letter)
    index match
      case value if value == (input.length - 1) => rotateLeftNTime(input.length - 4)
      case value if value == (input.length - 2) => rotateLeftNTime(input.length - 0)
      case value if value == (input.length - 3) => rotateLeftNTime(input.length - 5)
      case value if value == (input.length - 4) => rotateLeftNTime(input.length - 1)
      case value if value == (input.length - 5) => rotateLeftNTime(input.length - 6)
      case value if value == (input.length - 6) => rotateLeftNTime(input.length - 2)
      case value if value == (input.length - 7) => rotateLeftNTime(input.length - 7)
      case 0 => rotateLeftNTime(1)
      case _ => throw Exception("Not managed")

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