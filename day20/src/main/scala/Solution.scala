object Solution:

  def run(inputLines: Seq[String]): (String, String) =

    val ranges = inputLines.collect:
      case s"$first-$second" => MyRange(first.toLong, second.toLong)

    val rangesMerged = ranges.tail.foldLeft(List(ranges.head)):
      case (acc, newRange) =>
        val (toMerge, toLeaveUnTouched) = acc.partition(_ canMerge newRange)
        val updated = toMerge.foldLeft(newRange):
          case (internalAcc, internalRange) => (internalAcc merge internalRange).head
        (updated +: toLeaveUnTouched).sortBy(_.lowIncluded)

    val resultPart1 = rangesMerged.head.highIncluded + 1

    val higherLimit = ranges.length == 3 match
      case true => 9l
      case false => 4294967295l

    val resultPart2 =
      (rangesMerged ++: List(MyRange(higherLimit, higherLimit))).sliding(2, 1).collect:
        case List(first, second) => first nbBetween second
    .sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class MyRange(lowIncluded: Long, highIncluded: Long):
  private lazy val limits: List[Long] = List(lowIncluded, highIncluded)
  def nbBetween(other: MyRange): Long =
    (canMerge(other), endsAfter(other)) match
      case (true, _) => 0
      case (_ , true) => other.lowIncluded - highIncluded - 1
      case _ => highIncluded - other.lowIncluded - 1

  private def startsAfter(other: MyRange) = other.highIncluded < lowIncluded - 1
  private def endsAfter(other: MyRange) = other.lowIncluded > highIncluded + 1

  def canMerge(other: MyRange): Boolean = ! (startsAfter(other) || endsAfter(other))

  def merge(other: MyRange): List[MyRange] =
    if (endsAfter(other))
      List(this, other)
    else if (startsAfter(other))
      List(other, this)
    else
      val lower :: _ :: _ :: higher :: Nil = (limits ::: other.limits).sorted : @unchecked
      List(MyRange(lower, higher))
