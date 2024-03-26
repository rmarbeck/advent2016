object Solution:
  val TriangleMatcher = """(\d+) +(\d+) +(\d+)""".r.unanchored
  def run(inputLines: Seq[String]): (String, String) =

    val resultPart1 = inputLines.count(isMatching)

    val asTransposedArray = inputLines.map(_.split(" ").filterNot(_.isBlank)).toArray.transpose.flatten.view
    val resultPart2 = asTransposedArray.grouped(3).map(_.mkString(" ")).count(isMatching)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

  private def isMatching(str: String): Boolean =
    str match
      case TriangleMatcher(one, two, three) => Triangle(one, two, three).isValid

end Solution

case class Triangle(one: String, two: String, three: String):
  private val List(first, second, third) = List(one, two, three).map(_.toInt).sorted
  val isValid: Boolean = first + second > third
