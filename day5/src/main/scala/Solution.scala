package solution

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}

val parallelization = 63

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (resultPart1, resultPart2) = find(pekkoservice.DigitsService.getIterator(inputLines.head))
    //val (resultPart1, resultPart2) = find(digits(0, inputLines.head).iterator)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

type DigitsProvider = Iterator[String]

def digits(fromIndex: Int = 0, root: String): LazyList[String] =
  @tailrec
  def search(index: Int): (String, Int) =
    MD5.firstCharAfter5ZerosInHash(s"$root$index") match
      case Some(value) =>
        //println(s"$value $index")
        (value, index)
      case None => search(index + 1)
  val (foundChars, atIndex) = search(fromIndex)

  foundChars #:: digits(atIndex+1, root)

def digitsPar(fromIndex: Int = 0, root: String): LazyList[String] =

  def search(index: Int): (IndexedSeq[Option[(String, Int)]], Int) =
    import concurrent.ExecutionContext.Implicits.global
    import concurrent.duration.DurationInt

    val futures = (0 to parallelization).map:
      innerIndex => Future {
        MD5.firstCharAfter5ZerosInHash(s"$root${index+innerIndex}") match
          case Some(value) => Some((value, index+innerIndex))
          case None => None
      }
    val results = Future.sequence(futures)
    val result = Await.result(results, 2.seconds)
    result match
      case value if value.flatten.isEmpty => search(index + (parallelization+1))
      case _ => (result, index + parallelization)

  val values = search(fromIndex)._1.flatten.sortBy(_._2)
  val newIndex = search(fromIndex)._2

  digits(newIndex+1, root).prependedAll(values.map(_._1))

@tailrec
def find(provider: DigitsProvider, resultPart1: String = "", resultPart2: Array[Option[Char]] = Array.fill(8)(None)): (String, String) =
  def fromSixth(sixthValue: Char): Option[Int] =
    sixthValue.asDigit match
      case value if value < 8 => Some(value)
      case _ => None
  resultPart2.exists(_.isEmpty) match
    case false => (resultPart1, resultPart2.flatten.mkString)
    case true =>
      val Array(sixth, seventh, _*) = provider.next.toCharArray : @unchecked
      //println(s"Found !!!!!!!!!!!!!!! $sixth - $seventh")
      val updatedPart1 = resultPart1.length match
        case 8 => resultPart1
        case _ => s"$resultPart1$sixth"

      fromSixth(sixth).foreach:
        asIndex =>
          if (resultPart2(asIndex).isEmpty)
            resultPart2(asIndex) = Some(seventh)

      find(provider, updatedPart1, resultPart2)

object MD5:
  def firstCharAfter5ZerosInHash(s: String): Option[String] =
    def padLeft(str: String, size: Int, char: Char): String = s"${char.toString*(size-str.length)}$str"
    val m = java.security.MessageDigest.getInstance("MD5")
    m.update(s.getBytes)
    val firstBytes = m.digest.take(4)
    if (firstBytes(0) != 0 || firstBytes(1) != 0 || (firstBytes(2) & 0xff) > 15)
      None
    else
      val valueOfFirstBytes = firstBytes.drop(2).foldLeft(0l):
        case (acc, currentByte) => acc * 256 + (currentByte & 0xff)

      Some(padLeft(valueOfFirstBytes.toHexString, 3, '0'))

