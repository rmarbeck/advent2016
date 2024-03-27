object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val withoutSquareBrackets = """(\[.+\])|([^\[\]]+)""".r.unanchored
    val withinSquareBrackets = """\[([^\]]+)\]""".r.unanchored

    val resultPart1 = inputLines.count:
      line =>
        withinSquareBrackets.findAllMatchIn(line).flatMap(_.subgroups).exists(isAbba) match
          case true => false
          case false => line.split("""\[|\]""").exists(isAbba)

    val resultPart2 = inputLines.count:
      line =>
        val inHypernet = withinSquareBrackets.findAllMatchIn(line).flatMap(_.subgroups).toList
        val inSupernetOnly = line.split("""\[|\]""").filterNot(inHypernet contains _)
        (inHypernet.flatMap(extractAba).map(swapAba).toSeq intersect inSupernetOnly.flatMap(extractAba).toSeq).nonEmpty


    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

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