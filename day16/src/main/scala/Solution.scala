import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val Seq(input, lengthPart1, lengthPart2) = inputLines

    val resultPart1 = checkSum(doIt(input, lengthPart1.toInt))
    val resultPart2 = checkSum(doIt(input, lengthPart2.toInt))

    val result1 = s"${resultPart1}"
    val result2 = s"${resultPart2}"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def checkSum(input: String): String =
  val result = input.grouped(2).collect:
    case couple if couple.head == couple.last => '1'
    case _ => '0'
  .mkString

  result.length % 2 == 0 match
    case true => checkSum(result)
    case false => result

@tailrec
def doIt(a: String, size: Int): String =
  val b = a.reverse.replaceAll("1", "2").replaceAll("0", "1").replaceAll("2", "0")
  val result = s"${a}0${b}"
  if (result.length >=  size)
    result.take(size)
  else
    doIt(result, size)
