import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given listener: Listener = new Part1Listener
    val world =  World()

    val initialisation = inputLines.collect:
      case s"value ${chip} goes to bot ${bot}" => InitRule(world.getBot(bot.toInt), chip.toInt)

    inputLines.foreach:
      case s"bot ${sender} gives low to ${receiverType1} ${receiver1} and high to ${receiverType2} ${receiver2}" =>
        def getReceiver(typeOfReceiver: String, receiver: String): Destination =
          typeOfReceiver match
            case "output" => world.getOutput(receiver.toInt)
            case "bot" => world.getBot(receiver.toInt)
        val bot = world.getBot(sender.toInt)
        bot.setRule(getReceiver(receiverType1, receiver1), getReceiver(receiverType2, receiver2))
      case _ => ()

    initialisation.foreach:
      case InitRule(bot, chip) => bot.accept(chip)

    val resultPart1 = listener.getValue

    val result1 = s"${resultPart1.map(_.name).getOrElse(0)}"
    val result2 = s"${world.outputs.take(3).map(_._2.chips.head).product}"

    (s"${result1}", s"${result2}")

end Solution

trait Listener:
  def fire(bot: Bot): Unit
  def getValue: Option[Bot]

class Part1Listener extends Listener:
  private var value: Option[Bot] = None
  override def fire(bot: Bot): Unit =
    if (bot.chips.contains(61) && bot.chips.contains(17))
      value = Some(bot)

  override def getValue: Option[Bot] = value

type BotName = Int
type OutputName = Int

class World(using Listener):
  private val bots: mutable.Map[BotName, Bot] = mutable.HashMap[BotName, Bot]()
  val outputs: mutable.Map[OutputName, Output] = mutable.HashMap[OutputName, Output]()
  def getBot(name: BotName): Bot =
    bots.getOrElseUpdate(name, Bot(name, List()))
  def getOutput(name: OutputName): Output =
    outputs.getOrElseUpdate(name, Output(name, List()))

case class InitRule(bot: Bot, chip: Int)

trait Destination:
  def accept(chip: Int): Destination

class Output(val name: Int, var chips: List[Int]) extends Destination:
  override def accept(chip: Int): Destination =
    chips = chip +: chips
    this

class Bot(val name: Int, var chips: List[Int])(using Listener) extends Destination:
  private var lowDest: Option[Destination] = None
  private var highDest: Option[Destination] = None
  def setRule(lowDestination: Destination, highDestination: Destination) =
    lowDest = Some(lowDestination)
    highDest = Some(highDestination)

  override def accept(chip: Int): Bot =
    require(chips.length <= 1)
    chips = chip +: chips
    summon[Listener].fire(this)
    if (chips.length == 2)
      giveLower()
      giveHigher()
    this

  private def give(destination: Destination, toGive: Int): Bot =
    require(chips.nonEmpty)
    destination.accept(toGive)
    this.chips = this.chips.filterNot(_ == toGive)
    this

  private def giveLower(): Bot =
    require(lowDest.isDefined)
    give(lowDest.get, this.chips.min)

  private def giveHigher(): Bot =
    require(highDest.isDefined)
    give(highDest.get, this.chips.max)