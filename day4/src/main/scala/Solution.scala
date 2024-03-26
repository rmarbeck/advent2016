import Room.alphabet

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val RoomMatcher = """([a-z]+)|(\d+)""".r
    val validRooms = inputLines.view.collect:
      line =>
        val values = RoomMatcher.findAllIn(line).toList.reverse
        Room(values.head, values.tail.head, values.tail.tail:_*)
    .filter(_.isValid)

    val resultPart1 = validRooms
                          .map(_.sectorId)
                          .sum

    val resultPart2 = validRooms
                        .find(_.decoded.contains("north"))
                        .map(_.sectorId).getOrElse("N/A")

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

type LetterByFrequency = (Char, Int)

given Ordering[LetterByFrequency] with
  def compare(a: LetterByFrequency, b: LetterByFrequency): Int =
    a._2.compare(b._2) match
      case 0 => a._1.compare(b._1)
      case value => -value

case class Room(checksum: String, sectorId: Int, encryptedValues: String*):
  val isValid: Boolean =
    val frequency = encryptedValues.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    val sorted = frequency.map((key, value) => (key, value)).toList.sorted.map(_._1).take(5)

    sorted.mkString == checksum

  def rotate(char: Char): Char =
    alphabet((alphabet.indexOf(char) + sectorId) % 26)

  lazy val decoded: String =
    encryptedValues.map(_.map(rotate)).mkString(" ")

object Room:
  val alphabet = ('a' to 'z').mkString
  def apply(checksum: String, sectorId: String, encryptedValues: String*): Room =
    require(sectorId.toIntOption.isDefined)
    new Room(checksum, sectorId.toInt, encryptedValues:_*)