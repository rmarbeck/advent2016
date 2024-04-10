import scala.collection.mutable.{Map, HashMap}

object Solution:
  def run(inputLines: Seq[String]): (String, String) =


    val input = inputLines.head
    val nbRows = input.length match
      case 100 => (40, 400000)
      case value => (value, value)

    val (resultPart1, resultPart2)  = calcSafeRows(Row(input), nbRows)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

def calcSafeRows(current: Row, max: (Int, Int), nb: Int = 0, currentSafeCount: (Int, Int) = (0, 0)): (Int, Int) =
  nb match
    case rowNum if rowNum == max._2 => currentSafeCount
    case rowNum =>
      val newRow = current.next
      val safesInCurrent = current.safeCount
      if (rowNum < max._1)
        calcSafeRows(newRow, max, nb + 1, (currentSafeCount._1 + safesInCurrent, currentSafeCount._2 + safesInCurrent))
      else
        calcSafeRows(newRow, max, nb + 1, (currentSafeCount._1, currentSafeCount._2 + safesInCurrent))


case class Row(values: String):
  def safeCount: Int = values.count(_ == '.')

  def next: Row =
    val withSafeBorders = s".${values}."
    val newRowValues = withSafeBorders.sliding(3, 1).collect:
      case "^^." => '^'
      case ".^^" => '^'
      case "^.." => '^'
      case "..^" => '^'
      case _ => '.'
    .mkString
    Row(newRowValues)
