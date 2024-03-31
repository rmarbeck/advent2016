object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val withoutSquareBracketsStrict = """(?:\[[^\]]+\])?([^\[\]]*)(?:\[[^\]]+\])?""".r.unanchored
    val withinSquareBrackets = """\[([^\]]+)\]""".r.unanchored

    val (list1, list2) = inputLines.collect:
      line =>
        val hypernetGroups = withoutSquareBracketsStrict.findAllMatchIn(line).flatMap(_.subgroups).toList
        val supernetGroups = withinSquareBrackets.findAllIn(line).toList
        val part1 = hypernetGroups.exists(isAbba) match
          case true => ! supernetGroups.exists(isAbba)
          case false => false
        val part2 = (hypernetGroups.flatMap(extractAba).map(swapAba) intersect supernetGroups.filterNot(hypernetGroups contains _).flatMap(extractAba).toSeq).nonEmpty
        (part1, part2)
    .unzip

    val result1 = s"${list1.count(_ == true)}"
    val result2 = s"${list2.count(_ == true)}"

    (s"${result1}", s"${result2}")

end Solution

def isAbba(str: String): Boolean =
  str.toCharArray.sliding(4, 1).exists:
    case Array(first, second, third, fourth) => first == fourth && second == third && first != second

def extractAba(str: String): List[String] =
  str.toCharArray.sliding(3,1).toList.filter:
    case Array(first, second, third) => first == third && second != third
  .map(_.mkString)

def swapAba(str: String): String = s"${str(1)}${str(0)}${str(1)}"