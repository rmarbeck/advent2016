import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.parallel.*
import collection.parallel.CollectionConverters.IterableIsParallelizable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val genExtractor = """a ([a-z]+) generator""".r.unanchored
    val chipExtractor = """a ([a-z]+)-compatible microchip""".r.unanchored

    val initialFloors = inputLines.zipWithIndex.collect:
      case (s"The ${_} floor contains ${contents}.", row) =>
        val generators = genExtractor.findAllMatchIn(contents).map(matching => RTG(matching.subgroups.head))
        val microchips = chipExtractor.findAllMatchIn(contents).map(matching => Microchip(matching.subgroups.head))
        Floor(row + 1, (generators ++ microchips).toList)

    val resultPart1 = {
      val initialRtfPart1 = RTF(initialFloors.toArray)
      given RTF = initialRtfPart1
      val initialElevator = Elevator(1, Nil)
      val initialState = State(initialElevator, initialRtfPart1, 0)

      explore(List(initialState))
    }

    val nbAddedElementsPart2 = 2
    val forwardsBackWardsToMoveUPACoupleFromLevel1 = 12
    val resultPart2 = resultPart1 + nbAddedElementsPart2 * forwardsBackWardsToMoveUPACoupleFromLevel1

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def explore(states: List[State], explored: Set[String] = HashSet[String](), explorations: Int = 0, skipped: Int = 0): Int =
  states match
    case Nil => 0
    case head :: tail if head.isEnding => head.counter
    case head :: tail if explored.contains(head.toHash) => explore(tail, explored, explorations, skipped + 1)
    case head :: tail => explore(tail ::: head.next, explored + head.toHash, explorations+1, skipped)

case class RTF(floors: Array[Floor]):
  def moveFromFloorToElevator(level: Int, things: List[Thing]): RTF =
    val updatedFloors =
      floors.collect:
        case floor if floor.level == level => floor.copy(contents = floor.contents diff things)
        case floor => floor
    RTF(updatedFloors)

  def loadFromElevator(elevator: Elevator): RTF =
    val updatedFloors =
      floors.collect:
        case floor if floor.level == elevator.level => floor.copy(contents = floor.contents ::: elevator.inside)
        case floor => floor
    RTF(updatedFloors)

  def contentOfFloor(level: Int): List[Thing] = floors(level-1).contents

  override def toString: String = floors.reverse.map(_.toString).mkString("\n")

case class State(elevator: Elevator, rtf: RTF, counter: Int = 0):
  override def toString: String = s"[$counter] $elevator <->\n$rtf"
  lazy val toHash =
    val status = rtf.loadFromElevator(elevator).floors.map:
      floor =>
        s"${floor.level}${floor.contents.sortBy(_.toString).mkString}"
    .mkString
    s"${elevator.level}$status"


  private lazy val isValid: Boolean =
    val result = rtf.loadFromElevator(elevator).floors.forall:
      floor =>
        floor.contents.exists(_.isInstanceOf[RTG]) match
          case false => true
          case true =>
            val allMicrochipNamesAtThisLevel = floor.contents.collect:
              case microchip: Microchip => microchip.name
            allMicrochipNamesAtThisLevel.forall(current => floor.contents.count(_.name == current) == 2)
    result

  lazy val isEnding: Boolean = elevator.level == 4 && (1 to 3).forall(rtf.contentOfFloor(_).isEmpty)
  def next: List[State] =
    given updatedRtf: RTF = rtf.loadFromElevator(elevator)
    val emptiedElevator = elevator.empty
    emptiedElevator.loadableCombinations.par.flatMap:
      currentCombination =>
        val goUp = emptiedElevator.moveUpWith(currentCombination).map((newElevator, newRtf) => State(newElevator, newRtf, counter + 1))
        val goDown = emptiedElevator.moveDownWith(currentCombination).map((newElevator, newRtf) => State(newElevator, newRtf, counter + 1))
        List(goUp, goDown).flatten.filter(_.isValid)
    .toList

case class Floor(level: Int, contents: List[Thing]):
  override def toString: String = s"Floor [${level}] : ${contents.mkString(",")}"

case class Elevator(level: Int, inside: List[Thing]):
  def empty: Elevator = this.copy(inside = Nil)

  def moveUpWith(testInside: List[Thing])(using rtf: RTF): Option[(Elevator, RTF)] =
    require(inside.isEmpty)
    level match
      case 4 => None
      case _ => Some(Elevator(level = level + 1, inside = testInside), rtf.moveFromFloorToElevator(level, testInside))
  def moveDownWith(testInside: List[Thing])(using rtf: RTF): Option[(Elevator, RTF)] =
    require(inside.isEmpty)
    level match
      case 1 => None
      case _ => Some(Elevator(level = level - 1, inside = testInside), rtf.moveFromFloorToElevator(level, testInside))

  def loadableCombinations(using rtf: RTF): List[List[Thing]] =
    val allThings = inside ++: rtf.contentOfFloor(level)

    allThings.combinations(1).toList ++: allThings.combinations(2).toList


trait Thing(val name: String)

case class RTG(matter: String) extends Thing(matter):
  override def toString: String = s"RTG_$name"
case class Microchip(matter: String) extends Thing(matter):
  override def toString: String = s"Micro_$name"