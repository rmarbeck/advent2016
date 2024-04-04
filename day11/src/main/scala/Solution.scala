import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val initialFloors = inputLines.zipWithIndex.collect:
      case (s"The ${_} floor contains ${contents}.", row) =>
        val contentsOfFloor =
          contents.split(", ").flatMap(_.split(" and ")).collect:
            case s"${_} ${matter}-compatible microchip" => Microchip(matter)
            case s"${_} ${matter} generator" => RTG(matter)
        Floor(row + 1, contentsOfFloor.toList)

    val initialRtf = RTF(initialFloors.toArray)
    given RTF = initialRtf
    val initialElevator = Elevator(1, Nil)
    val initialState = State(initialElevator, initialRtf , 0)

    println(initialRtf)

    println(explore(List(initialState)))

    //
    // Code is here
    //



    val result1 = s""
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def explore(states: List[State]): Int =
  states match
    case Nil => 0
    case head :: tail if head.isEnding => head.counter
    case head :: tail =>
      //println(s"$head")
      explore((tail ::: head.next).sortBy(_.counter))

case class RTF(floors: Array[Floor]):
  def unloadFromFloor(level: Int, things: List[Thing]): RTF =
    val updatedFloors =
      floors.zipWithIndex.collect:
        case (floor, index) if index == level - 1 => floor.copy(contents = floor.contents diff things)
        case (floor, _) => floor
    RTF(updatedFloors)

  def loadFromElevator(elevator: Elevator): RTF =
    val updatedFloors =
      floors.zipWithIndex.collect:
        case (floor, index) if index == elevator.level - 1 => floor.copy(contents = floor.contents ::: elevator.inside)
        case (floor, _) => floor
    RTF(updatedFloors)

  def contentOfFloor(level: Int): List[Thing] = floors(level-1).contents

  override def toString: String = floors.reverse.map(_.toString).mkString("\n")

trait Thing(val name: String)

case class RTG(matter: String) extends Thing(matter)
case class Microchip(matter: String) extends Thing(matter)

case class State(elevator: Elevator, rtf: RTF, counter: Int = 0):
  override def toString: String = s"[$counter] $elevator <->\n$rtf"
  lazy val isValid: Boolean =
    val result = rtf.floors.forall:
      floor =>
        val elementsFromElevator =
          elevator.level == floor.level match
            case true => elevator.inside
            case false => Nil
        val allElementsAtThisLevel = floor.contents ::: elementsFromElevator
        val allMicrochipNamesAtThisLevel = allElementsAtThisLevel.collect:
          case microchip: Microchip => microchip.name
        val rtgExistAtThisLevel = allElementsAtThisLevel.exists(_.isInstanceOf[RTG])
        (! rtgExistAtThisLevel) || allMicrochipNamesAtThisLevel.forall(current => allElementsAtThisLevel.count(_.name == current) == 2)

    result

  lazy val isEnding: Boolean = (1 to 3).forall(rtf.contentOfFloor(_).isEmpty)
  lazy val hasNext: Boolean = isEnding
  def next: List[State] =
    given updatedRtf: RTF = rtf.loadFromElevator(elevator)
    println(s"$updatedRtf")
    println(s"${elevator.empty}")
    elevator.empty.loadableCombinations.flatMap:
      currentCombination =>
        val goUp = elevator.moveUpWith(currentCombination).map((newElevator, newRtf) => State(newElevator, newRtf, counter + 1))
        val goDown = elevator.moveDownWith(currentCombination).map((newElevator, newRtf) => State(newElevator, newRtf, counter + 1))
        List(goUp, goDown).flatten
    .filter(_.isValid)

case class Floor(level: Int, contents: List[Thing]):
  override def toString: String = s"Floor [${level}] : ${contents.mkString(",")}"

case class Elevator(level: Int, inside: List[Thing]):
  def moveUp: Elevator = this.copy(level = level + 1)
  def moveDown: Elevator = this.copy(level = level - 1)

  def empty: Elevator = this.copy(inside = Nil)

  def moveUpWith(testInside: List[Thing])(using rtf: RTF): Option[(Elevator, RTF)] =
    level match
      case 4 => None
      case _ => Some(this.copy(level = level + 1, inside = testInside), rtf.unloadFromFloor(level, testInside))
  def moveDownWith(testInside: List[Thing])(using rtf: RTF): Option[(Elevator, RTF)] =
    level match
      case 1 => None
      case _ =>
        if (testInside.count(_.isInstanceOf[RTG]) > 0)
          None
        else
          Some(this.copy(level = level - 1, inside = testInside), rtf.unloadFromFloor(level, testInside))

  def loadableCombinations(using rtf: RTF): List[List[Thing]] =
    val allThings = inside ::: rtf.contentOfFloor(level)
    val allMicrochips =
      allThings.collect:
        case microchip: Microchip => microchip

    val result = allMicrochips.flatMap:
      microchip =>
        val allOther = allThings.filterNot(_ == microchip)
        List[Thing](microchip) :: allOther.map(thing => List(thing, microchip))

    println(s"$result")
    result