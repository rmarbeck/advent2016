import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val input = inputLines.head

    val resultPart1 = update(input)

    val resultPart2 = updateRec(input)

    val result1 = s"${resultPart1.length}"
    val result2 = s"${resultPart2}"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def update(input: String, output: String = ""): String =
  input match
    case "" => output
    case s"(${nbChars}x${repeat})${tail}" =>
      val (toRepeat, toLeave) = tail.splitAt(nbChars.toInt)
      update(toLeave, s"${output}${toRepeat*repeat.toInt}")
    case value => update(input.tail, s"${output}${input.head}")

def updateRec(input: String, length: Long = 0l): Long =
  input match
    case "" => length
    case s"(${nbChars}x${repeat})${tail}" =>
      val (toRepeat, toLeave) = tail.splitAt(nbChars.toInt)
      updateRec(toLeave, length + updateRec(toRepeat)*repeat.toLong)
    case value => updateRec(input.tail, length + 1)
