object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val initialGrid: Grid = inputLines.head match
      case s"rect 3x2" => MutableGrid(7, 3)
      case _ => MutableGrid(50, 6)

    val resultPart1 = inputLines.foldLeft(initialGrid):
      case (grid, s"rect ${width}x${height}") => grid.turnOn(width.toInt, height.toInt)
      case (grid, s"rotate column x=${col} by ${pixels}") => grid.rotateCol(col.toInt, pixels.toInt)
      case (grid, s"rotate row y=${row} by ${pixels}") => grid.rotateRow(row.toInt, pixels.toInt)

    val result1 = s"${resultPart1.howManyOn}"
    val result2 = s"\n$resultPart1"

    (s"${result1}", s"${result2}")

end Solution

trait Grid(width: Int, height: Int):
  def dataAsIterable: Iterable[Iterable[Boolean]]

  override def toString: String =
    dataAsIterable.map:
        _.map:
          case true => '#'
          case false => '.'
        .mkString
    .mkString("\n")

  def howManyOn: Int = dataAsIterable.flatten.count(_ == true)

  def turnOn(width: Int, height: Int): Grid
  def rotateCol(col: Int, pixels: Int): Grid
  def rotateRow(row: Int, pixels: Int): Grid


class MutableGrid(width: Int, height: Int) extends Grid(width, height):
  val data: Array[Array[Boolean]] = Array.fill(height, width)(false)

  override def dataAsIterable: Iterable[Iterable[Boolean]] =
    data.map(_.toIndexedSeq).toIndexedSeq

  private def turnOffRow(row: Int): Unit =
    for col <- 0 until width
    do data(row)(col) = false

  private def turnOffCol(col: Int): Unit =
    for row <- 0 until height
      do data(row)(col) = false

  override def rotateCol(col: Int, pixels: Int): MutableGrid =
    require(col <= this.width - 1)
    val nextOnIndexes =
      for row <- 0 until height if data(row)(col) == true
      yield
        (row + pixels) % height

    turnOffCol(col)

    nextOnIndexes.foreach:
      data(_)(col) = true
    this

  override def rotateRow(row: Int, pixels: Int): MutableGrid =
    require(row <= this.height - 1)
    val nextOnIndexes =
      for col <- 0 until width if data(row)(col) == true
      yield
        (col + pixels) % width

    turnOffRow(row)

    nextOnIndexes.foreach:
      data(row)(_) = true
    this

  override def turnOn(widthOn: Int, heightOn: Int): MutableGrid =
    require(widthOn <= this.width)
    require(heightOn <= this.height)
    for
      row <- 0 until heightOn
      col <- 0 until widthOn
    do
      data(row)(col) = true
    this


case class ImmutableGrid(width: Int, height: Int, data: Vector[Vector[Boolean]]) extends Grid(width, height):
  val dataAsIterable: Iterable[Iterable[Boolean]] = data

  override def turnOn(widthOn: Int, heightOn: Int): ImmutableGrid =
    require(widthOn <= this.width)
    require(heightOn <= this.height)
    val newData =
      (for
        row <- 0 until height
        col <- 0 until width
      yield
        if (row < heightOn && col < widthOn)
          true
        else
          data(row)(col)
        ).grouped(width).toVector
    this.copy(data = newData.map(_.toVector))

  override def rotateRow(row: Int, pixels: Int): ImmutableGrid =
    require(row <= this.height - 1)
    val newData = data.zipWithIndex.collect:
      case (initialValues, currentRow) if currentRow != row => initialValues
      case (initialValues, _) =>
        val toTurnOnIndexes = initialValues.zipWithIndex.filter(_._1 == true).map((_ ,index) => (index + pixels) % width)
        (0 until width).map:
          col => toTurnOnIndexes.contains(col)
        .toVector

    this.copy(data = newData.map(_.toVector))

  override def rotateCol(col: Int, pixels: Int): ImmutableGrid =
    require(col <= this.width - 1)
    val toTurnOnIndexes = data.map(_(col)).zipWithIndex.filter(_._1 == true).map((_ ,index) => (index + pixels) % height)
    val newData = data.zipWithIndex.collect:
      case (initialValues, currentRow) if toTurnOnIndexes.contains(currentRow) =>
        (initialValues.take(col) :+ true) ++ initialValues.drop(col + 1)
      case (initialValues, _) =>
        (initialValues.take(col) :+ false) ++ initialValues.drop(col + 1)

    this.copy(data = newData.map(_.toVector))

object ImmutableGrid:
  def apply(width: Int, height: Int): ImmutableGrid =
    val emptyVectors =
      (for
        row <- 0 until height
      yield
        (for
          col <- 0 until width
        yield
          false).toVector).toVector

    new ImmutableGrid(width, height, emptyVectors)