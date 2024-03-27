object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    digits(0, inputLines.head).take(7).foreach(print)
    //
    // Code is here
    //



    val result1 = s""
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

def digits(fromIndex: Int = 0, root: String): LazyList[Char] =
  def search(index: Int): (Char, Int) =
    MD5.firstCharAfterZerosInHash(s"$root$index", 5) match
      case Some(value) => (value, index)
      case None => search(index + 1)
  val (foundChar, atIndex) = search(fromIndex)

  foundChar #:: digits(atIndex+1, root)


object MD5 {
  def firstCharAfterZerosInHash(s: String, nbOfZeros: Int): Option[Char] =
    val m = java.security.MessageDigest.getInstance("MD5")
    m.update(s.getBytes)
    val firstBytes = m.digest.take(nbOfZeros / 2 + 1)
    val valueOfFirstBytes = firstBytes.foldLeft(0):
      case (acc, currentByte) => acc * 256 + (currentByte & 0xff)

    valueOfFirstBytes match
      case value if value < 16 => value.toHexString.headOption
      case _ => None


  def hashNumberOfLeadingZerosBasedOnFirstBytes(s: String, maxZerosToFind: Int): Int = {
    val nbOfBytes = math.ceil(maxZerosToFind / 2d).toInt
    val m = java.security.MessageDigest.getInstance("MD5")
    m.update(s.getBytes)
    val firstBytes = m.digest.take(maxZerosToFind/2)
    val valueOfFirstBytes = firstBytes.foldLeft(0):
      case (acc, currentByte) => acc * 256 + (currentByte & 0xff).toInt

    valueOfFirstBytes match
      case 0 => maxZerosToFind
      case value if value < 16 => maxZerosToFind - 1
      case _ => -1
  }

  def hashNumberOfLeadingZeros(s: String): Int = {
    val m = java.security.MessageDigest.getInstance("MD5")
    m.update(s.getBytes)
    val valueAsBytes = m.digest
    32 - new java.math.BigInteger(1, valueAsBytes).toString(16).length
  }
  def hash(s: String): String = {
    val m = java.security.MessageDigest.getInstance("MD5")
    val b = s.getBytes("UTF-8")
    m.update(b, 0, b.length)
    val result = new java.math.BigInteger(1, m.digest()).toString(16)
    32-result.length match
      case 0 => result
      case value => "0"*value + result
  }
}