object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val transposed = inputLines.map(_.toCharArray).toArray.transpose

    val (resultPart1, resultPart2) = buildResult2(transposed)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def buildResult2(input: Array[Array[Char]]): (String, String) =
  input.foldLeft(("", "")):
    case ((part1, part2), line) =>
      val ranked = line.groupMapReduce(identity)(_ => 1)(_ + _).toArray.sortBy(_._2).collect(_._1)
      (s"$part1${ranked.last}", s"$part2${ranked.head}")
