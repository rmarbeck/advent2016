import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val matrix = explore(inputLines.toArray.map(_.toCharArray))

    val result1 = s"${matrix.withPermutationsPart1(0)}"
    val result2 = s"${matrix.withPermutationsPart2(0)}"

    (s"${result1}", s"${result2}")

end Solution

case class Position(row: Int, col: Int):
  lazy val up: Position = this.copy(row = row - 1)
  lazy val down: Position = this.copy(row = row + 1)
  lazy val left: Position = this.copy(col = col - 1)
  lazy val right: Position = this.copy(col = col + 1)

  lazy val next: List[Position] = List(up, down, left, right)

type Node = (Int, Position)
type Distance = (Position, Int)

def explore(zone: Array[Array[Char]]): Matrix =

  import scala.collection.mutable.Map
  val distances: Map[Position, Array[Array[Int]]] = Map[Position, Array[Array[Int]]]()

  def distanceMapCached(from: Position): Array[Array[Int]] =
    distances.getOrElseUpdate(from, distanceMap(from))

  def distanceMap(from: Position): Array[Array[Int]] =
    @tailrec
    def updateDistanceMap(current: List[Distance], distances: Array[Array[Int]]): Array[Array[Int]] =
      current match
        case Nil => distances
        case head :: tail =>
          val connected = head._1.next
          val newToExplore = connected.filter:
            case Position(row, col) if distances.isDefinedAt(row) && distances(row).isDefinedAt(col) && distances(row)(col) > (head._2 + 1) => true
            case _ => false
          .map(currentPosition => (currentPosition, head._2 + 1))
          newToExplore.foreach:
            case (position, distance) => distances(position.row)(position.col) = distance

          updateDistanceMap((tail ++: newToExplore), distances)


    val initializedDistanceMap: Array[Array[Int]] =
      Array.tabulate(zone.length, zone(0).length):
        case (row, col) if row == from.row && col == from.col => 0
        case (row, col) if zone(row)(col) != '#' => Int.MaxValue
        case _ => -1
    updateDistanceMap(List((from, 0)), initializedDistanceMap)

  def calcDistanceBetween(from: Node, to: Node): Int =
    val distanceToFrom = distanceMapCached(from._2)
    distanceToFrom(to._2.row)(to._2.col)


  val nodes =
    for
      row <- zone.indices
      col <- zone(0).indices
      if zone(row)(col).toString.toIntOption.isDefined
    yield
      (zone(row)(col).asDigit, Position(row, col))

  val matrix = Matrix(nodes.length)

  nodes.zipWithIndex.foreach:
    case (currentNode, index) =>
      nodes.drop(index+1).map:
        innerNode => matrix.addDistance(currentNode._1, innerNode._1, calcDistanceBetween(currentNode, innerNode))

  matrix

class Matrix(val size: Int):
  override def toString: String = data.map(_.mkString("\t")).mkString("\n")
  private val data: Array[Array[Int]] = Array.fill(size, size)(0)
  def distanceBetween(first: Int, second: Int): Int =
    require(first < size)
    require(second < size)
    data(first)(second)
  def addDistance(first: Int, second: Int, distance: Int): Matrix =
    require(first < size)
    require(second < size)
    data(first)(second) = distance
    data(second)(first) = distance
    this

  def withPermutationsPart1(from: Int): Int =
    given Matrix = this
    (0 until this.size).filterNot(_ == from).permutations.map(current => Path(current.toList :+ from).steps).min

  def withPermutationsPart2(from: Int): Int =
    given Matrix = this
    (0 until this.size).filterNot(_ == from).permutations.map(current => Path((from +: current.toList) :+ from).steps).min


case class Path(jumps: List[Int])(using matrix: Matrix):
  override def toString: String = s"${jumps.reverse.mkString("<->")} ($steps)"
  lazy val steps: Int = jumps.sliding(2, 1).collect:
    case List(first, second) => matrix.distanceBetween(first, second)
  .sum
  lazy val isFull: Boolean = jumps.distinct.length == matrix.size
  lazy val isFullAndBack: Boolean = isFull && jumps.head == 0
  lazy val next: List[Path] =
    isFull match
      case true => List(Path(0 +: jumps))
      case false =>
        (0 until matrix.size).filterNot(_ == jumps.head).toList.collect:
          case 0 => Path(0 +: jumps)
          case current if !jumps.mkString.contains(s"$current${jumps.head}") => Path(current +: jumps)
