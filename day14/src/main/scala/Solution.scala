import scala.annotation.tailrec
import scala.concurrent.{Await, Future}

val part2Loops = 2016

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val salt = inputLines.head

    val resultPart1 = {
      given Hasher = (str: String) => MD5Cached.hashPart1(str)
      findPrime(salt, 0, Nil, 64)
    }

    val resultPart2 = {
      given Hasher = (str: String) => MD5Cached.hashPart2(str)

      findPrime(salt, 0, Nil, 64)
    }


    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

type Hasher = String => String

val primeKeyExtractor = """(.)\1{2}""".r.unanchored
val secondaryKeyExtractor = """(.)\1{4}""".r.unanchored

extension (str: String)
  def matchedPrimeKey: Option[Char] = primeKeyExtractor.findFirstMatchIn(str).map(_.subgroups.head.head)
  def matchedSecondaryKey: Option[Char] = secondaryKeyExtractor.findFirstMatchIn(str).map(_.subgroups.head.head)

@tailrec
def findPrime(key: String, currentIndex: Int, results: List[Int], indexSearched: Int)(using hasher: Hasher): Int =
  results.length == indexSearched match
    case true => currentIndex - 1
    case false =>
      hasher.apply(s"$key$currentIndex").matchedPrimeKey match
        case Some(foundChar) =>
          findSecondaryPar(key, currentIndex + 1, currentIndex + 1, foundChar) match
            case true => findPrime(key, currentIndex + 1, currentIndex +: results, indexSearched)
            case false => findPrime(key, currentIndex + 1, results, indexSearched)
        case None => findPrime(key, currentIndex + 1, results, indexSearched)

@tailrec
def findSecondary(key: String, startingIndex: Int, currentIndex: Int, charToFind: Char)(using hasher: Hasher): Boolean =
  if (currentIndex > startingIndex + 999)
    false
  else
    hasher.apply(s"$key$currentIndex").matchedSecondaryKey match
      case Some(repeated) if repeated == charToFind => true
      case _ => findSecondary(key, startingIndex, currentIndex + 1, charToFind)

def findSecondaryPar(key: String, startingIndex: Int, currentIndex: Int, charToFind: Char)(using hasher: Hasher): Boolean =
  import concurrent.ExecutionContext.Implicits.global
  import concurrent.duration.DurationInt
  def findInRange(start: Int, end: Int): Boolean =
    val futures = (start to end).map:
      innerIndex =>
        Future {
          hasher.apply(s"$key${startingIndex + innerIndex}").matchedSecondaryKey match
            case Some(repeated) if repeated == charToFind => true
            case _ => false
        }
    val results = Future.sequence(futures)
    val result = Await.result(results, 2.seconds)
    result.exists(_ == true)

  (0 to 999).grouped(200).map((current) => findInRange(current.start, current.end)).exists(_ == true)

object MD5:
  def hash(s: String): String =
    val m = java.security.MessageDigest.getInstance("MD5")
    val b = s.getBytes("UTF-8")
    m.update(b, 0, b.length)
    val result = new java.math.BigInteger(1, m.digest()).toString(16)
    32 - result.length match
      case 0 => result
      case value => "0" * value + result

  def hashPart2(s: String): String =
    def loop(toHash: String, index: Int): String =
      index < part2Loops match
        case true => loop(hash(toHash), index + 1)
        case false => toHash
    loop(s, 0)

object MD5Cached:
  import scala.collection.mutable.Map
  private val part1: Map[String, String] = Map()
  private val part2: Map[String, String] = Map()


  def hashPart1(s: String): String =
    part1.getOrElseUpdate(s, hashPart1Uncached(s))

  private def hashPart1Uncached(s: String): String =
      val m = java.security.MessageDigest.getInstance("MD5")
      val b = s.getBytes("UTF-8")
      m.update(b, 0, b.length)
      val result = new java.math.BigInteger(1, m.digest()).toString(16)
      32 - result.length match
        case 0 => result
        case value => "0" * value + result

  def hashPart2(s: String): String =
    part2.getOrElseUpdate(s, hashPart2Uncached(s))

  private def hashPart2Uncached(s: String): String =
    @tailrec
    def loop(toHash: String, index: Int): String =
      index < 2017 match
        case true => loop(hashPart1Uncached(toHash), index + 1)
        case false => toHash

    loop(hashPart1(s), 1)