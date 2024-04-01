object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val withoutSquareBracketsStrict = """(?:\[[^\]]+\])?([^\[\]]*)(?:\[[^\]]+\])?""".r.unanchored
    val withinSquareBrackets = """\[([^\]]+)\]""".r.unanchored

    val (resultPart1, resultPart2) = inputLines.foldLeft((0, 0)):
      case ((acc1, acc2), line) =>
        val hypernetGroups = withoutSquareBracketsStrict.findAllMatchIn(line).flatMap(_.subgroups).toSet
        val supernetGroups = withinSquareBrackets.findAllIn(line).toSet
        val part1: Boolean =
          hypernetGroups.exists(isAbba) match
            case true => ! supernetGroups.exists(isAbba)
            case false => false
        val part2: Boolean = (hypernetGroups.flatMap(extractAba).map(swapAba) intersect supernetGroups.flatMap(extractAba)).nonEmpty
        (acc1 + part1, acc2 + part2)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def isAbba(str: String): Boolean =
  str.toCharArray.sliding(4, 1).exists:
    case Array(first, second, third, fourth) => first == fourth && second == third && first != second

def extractAba(str: String): Iterator[String] =
  str.toCharArray.sliding(3,1).collect:
    case matchingValues @ Array(first, second, third) if first == third && second != third => matchingValues.mkString

def swapAba(str: String): String = s"${str(1)}${str(0)}${str(1)}"

extension (i: Int)
  def +(boolean: Boolean): Int =
    i + (boolean match
      case true => 1
      case false => 0)