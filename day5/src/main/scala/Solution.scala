import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (resultPart1, resultPart2) = find(digits(0, inputLines.head))

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def digits(fromIndex: Int = 0, root: String): LazyList[String] =
  @tailrec
  def search(index: Int): (String, Int) =
    MD5.firstCharAfter5ZerosInHash(s"$root$index") match
      case Some(value) => (value, index)
      case None => search(index + 1)
  val (foundChars, atIndex) = search(fromIndex)

  foundChars #:: digits(atIndex+1, root)

@tailrec
def find(provider: LazyList[String], resultPart1: String = "", resultPart2: Array[Option[Char]] = Array.fill(8)(None)): (String, String) =
  def fromSixth(sixthValue: Char): Option[Int] =
    sixthValue.asDigit match
      case value if value < 8 => Some(value)
      case _ => None
  resultPart2.exists(_.isEmpty) match
    case false => (resultPart1, resultPart2.flatten.mkString)
    case true =>
      val Array(sixth, seventh, _*) = provider.head.toCharArray : @unchecked
      val updatedPart1 = resultPart1.length match
        case 8 => resultPart1
        case _ => s"$resultPart1$sixth"

      fromSixth(sixth).foreach:
        asIndex =>
          if (resultPart2(asIndex).isEmpty)
            resultPart2(asIndex) = Some(seventh)

      find(provider.tail, updatedPart1, resultPart2)

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
