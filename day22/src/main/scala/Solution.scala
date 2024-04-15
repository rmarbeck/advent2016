object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val sizesParser = """([^ ]+)""".r.unanchored

    val nodes = inputLines.collect:
      case s"/dev/grid/node-x${x}-y${y} $end" =>
        val sizes = sizesParser.findAllMatchIn(end).map(_.subgroups.head).toList : @unchecked
        Node(List(x, y) ::: sizes)

    val resultPart1 =
      nodes.map:
        current =>
          current.used.inBytes match
            case 0 => 0
            case used =>
              nodes.filterNot(_ == current).count:
                _.available.inBytes >= used
      .sum

    //
    // Code is here
    //

    /*
    Filesystem              Size  Used  Avail  Use%
/dev/grid/node-x0-y0     85T   65T    20T   76%
/dev/grid/node-x0-y1     93T   66T    27T   70%
/dev/grid/node-x0-y2     86T   65T    21T   75%
     */


    val result1 = s"$resultPart1"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

class Size(val inBytes: Long):
  override def toString: String = s"$inBytes"

object Size:
  def unapply(str: String): Size =
    str match
      case s"${value}T" => Size(value.toLong * 1024 * 1024 * 1024)
      case _ => throw Exception("Not supported")


case class Node(x: Int, y : Int, total: Size, used: Size, available: Size, percentage: Int)

object Node:
  def apply(valuesAsString: List[String]): Node =
    new Node(valuesAsString(0).toInt, valuesAsString(1).toInt, Size.unapply(valuesAsString(2)), Size.unapply(valuesAsString(3)), Size.unapply(valuesAsString(4)), valuesAsString(5).dropRight(1).toInt)