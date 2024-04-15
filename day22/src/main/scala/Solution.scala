object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val SizesExtractor = """ +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+)%.*""".r.unanchored

    val nodes = inputLines.collect:
      case s"/dev/grid/node-x${x}-y${y} $end" =>
        val SizesExtractor(SizeExtractor(total), SizeExtractor(used), SizeExtractor(available), percentage) = end : @unchecked
        Node(x.toInt, y.toInt, total, used, available, percentage.toInt)

    val resultPart1 =
      nodes.map:
        current =>
          current.used.inBytes match
            case 0 => 0
            case used =>
              nodes.filterNot(_ == current).count:
                _.available.inBytes >= used
      .sum

    val resultPart2 = solvePart2(nodes)


    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def solvePart2(nodes: Seq[Node]): Int =
  lazy val getInitialEmpty: Node = nodes.find(_.isEmpty).get
  lazy val findLowestXYTooLargeNode: Node = nodes.filter(_.used.intTBytes > 400).sortBy(_.x).head
  lazy val distanceFromInitialEmptyToTurnAround: Int =
    val (emptyX, emptyY) = getInitialEmpty.coords
    val List(cornerX, cornerY) = findLowestXYTooLargeNode.coords.toList.map(_ - 1)

    (emptyX - cornerX).abs + (emptyY - cornerY).abs

  lazy val distanceFromCornerToData: Int =
    val (dataX, dataY) = (nodes.map(_.x).max - 1, 0)
    val List(cornerX, cornerY) = findLowestXYTooLargeNode.coords.toList.map(_ - 1)

    (dataX - cornerX + 1).abs + (dataY - cornerY + 1).abs

  lazy val nbStepsToMoveDataLeft: Int = 5

  lazy val stepsToMoveDataInFrontOfGoal: Int =
    val dataX = nodes.map(_.x).max
    (dataX - 1) * nbStepsToMoveDataLeft

  distanceFromInitialEmptyToTurnAround + distanceFromCornerToData + stepsToMoveDataInFrontOfGoal + 1


class NodesHolder(val nodes: Seq[Node]):
  def getEmptyOne: Node = nodes.find(_.isEmpty).get
  def getAdjacent(node: Node): Seq[Node] =
    nodes.filter:
      case current if current.x == node.x && (current.y == node.y + 1 || current.y == node.y - 1) => true
      case current if current.y == node.y && (current.x == node.x + 1 || current.x == node.x - 1) => true
      case _ => false

class Size(val inBytes: Long):
  lazy val intTBytes = inBytes/1024/1024/1024
  override def toString: String = s"${intTBytes}T"

object SizeExtractor:
  def unapply(str: String): Option[Size] =
    str match
      case s"${value}T" => Some(Size(value.toLong * 1024 * 1024 * 1024))
      case _ => None

case class Node(x: Int, y : Int, total: Size, used: Size, available: Size, percentage: Int):
  lazy val coords: (Int, Int) = (x, y)
  def isEmpty: Boolean = used.inBytes == 0
  def canMoveIn(nodes: Seq[Node]): Seq[Node] =
    this.isEmpty match
      case false => Seq()
      case true =>
        nodes.filter(_.used.inBytes <= this.available.inBytes)
