import scala.annotation.tailrec

type InstructionSet = Array[Instruction]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given Registers = Registers()
    given Signal = Signal()
    given InstructionSet = parseInstructions(inputLines)

    val nbOfInstructionsToProcess = 100_000

    val result1 = s"${search(0, nbOfInstructionsToProcess)}"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

def parseInstructions(rawInstructions: Seq[String])(using registers: Registers, signal: Signal): InstructionSet =
  rawInstructions.toArray.collect:
    case s"cpy ${input} ${to}" =>
      input.toIntOption match
        case Some(value) => CopyInt(value, to.asRegister)
        case None => CopyRegister(input.asRegister, to.asRegister)
    case s"inc ${x}" => Inc(x.asRegister)
    case s"dec ${x}" => Dec(x.asRegister)
    case s"tgl ${x}" => Toggle(x.asRegister)
    case s"jnz ${x} ${to}" =>
      (x.toIntOption, to.toIntOption) match
        case (Some(xValue), Some(toValue)) => JNZInt(xValue, toValue)
        case (Some(xValue), None) => JNZToRegister(xValue, to.asRegister)
        case (None, Some(toValue)) => JNZRegister(x.asRegister, toValue)
        case (None, None) => JNZFullRegister(x.asRegister, to.asRegister)
    case s"out ${x}" =>
      x.toIntOption match
        case Some(xValue) => OutInt(xValue)
        case None => OutRegister(x.asRegister)

@tailrec
def search(current: Int, maxInstructions: Int)(using registers: Registers, signal: Signal, instructionSet: InstructionSet): Int =
  registers.reset
  signal.reset
  registers.byName("a").set(current)
  process(instructionSet, 0, maxInstructions)
  if (signal.isGood)
    current
  else
    search(current+1, maxInstructions)


@tailrec
def process(instructions: Array[Instruction], index: Int = 0, loopRemaining: Int)(using registers: Registers): Int =
  loopRemaining > 0 && instructions.isDefinedAt(index) match
    case false => registers.byName("a").get
    case true =>
      instructions(index).apply(instructions, index) match
        case None => process(instructions, index + 1, loopRemaining - 1)
        case Some(value) => process(instructions, index + value, loopRemaining - 1)

extension (str: String)
  def asRegister(using Registers): Register = summon[Registers].byName(str)

class Signal:
  override def toString: String = data.mkString
  private var data: List[Int] = Nil
  def reset = data = Nil
  def add(signal: Int) = data = signal +: data
  def isGood: Boolean =
    val firsts = s"${data(0)}${data(1)}"
    data.grouped(2).filter(_.length == 2).forall(current => s"${current(0)}${current(1)}" == firsts)

class Registers:
  def reset: Unit = values.foreach((_, register) => register.reset)

  override def toString: String = values.mkString(" - ")

  private val values: Map[String, Register] = Map("a" -> Register(), "b" -> Register(), "c" -> Register(), "d" -> Register())
  def byName(name: String): Register = values(name)

trait Instruction:
  def toggle: Instruction
  def changeInstructionSet(instructions: Array[Instruction], currentIndex: Int): Unit = ()
  def next: Option[Int] = None
  def update(): Unit = ()
  final def apply(instructions: Array[Instruction], currentIndex: Int): Option[Int] =
    changeInstructionSet(instructions, currentIndex)
    update()
    next

case class CopyFullInt(x: Int, to: Int) extends Instruction:
  override def toggle: Instruction = JNZInt(x, to)
case class CopyToInt(x: Register, to: Int) extends Instruction:
  override def toggle: Instruction = JNZRegister(x, to)
case class CopyInt(x: Int, to: Register) extends Instruction:
  override def toggle: Instruction = JNZToRegister(x, to)
  override def update(): Unit = to.copy(x)
case class CopyRegister(x: Register, to: Register) extends Instruction:
  override def toggle: Instruction = JNZFullRegister(x, to)
  override def update(): Unit = to.copy(x.get)
case class Inc(register: Register) extends Instruction:
  override def update(): Unit = register.inc
  override def toggle: Instruction = Dec(register)
case class Dec(register: Register) extends Instruction:
  override def update(): Unit = register.dec
  override def toggle: Instruction = Inc(register)
case class JNZInt(x: Int, to: Int) extends Instruction:
  override def toggle: Instruction = CopyFullInt(x, to)
  override def next: Option[Int] =
    x match
      case 0 => None
      case _ => Some(to)
case class JNZRegister(x: Register, to: Int) extends Instruction:
  override def toggle: Instruction = CopyToInt(x, to)
  override def next: Option[Int] = JNZInt(x.get, to).next
case class JNZToRegister(x: Int, to: Register) extends Instruction:
  override def toggle: Instruction = CopyInt(x, to)
  override def next: Option[Int] = JNZInt(x, to.get).next
case class JNZFullRegister(x: Register, to: Register) extends Instruction:
  override def toggle: Instruction = CopyRegister(x, to)
  override def next: Option[Int] = JNZInt(x.get, to.get).next

case class OutInt(x: Int)(using signal: Signal) extends Instruction:
  override def toggle: Instruction = this
  override def update(): Unit = signal.add(x)

case class OutRegister(x: Register)(using signal: Signal) extends Instruction:
  override def toggle: Instruction = this
  override def update(): Unit = signal.add(x.get)

case class Toggle(register: Register) extends Instruction:
  override def toggle: Instruction = Inc(register)
  override def changeInstructionSet(instructions: Array[Instruction], currentIndex: Int): Unit =
    instructions.isDefinedAt(currentIndex + register.get) match
      case false => ()
      case true => instructions(currentIndex + register.get) = instructions(currentIndex + register.get).toggle


class Register:
  override def toString: String = s"reg $value"
  private var value = 0
  def set(newValue: Int): Register =
    value = newValue
    this
  def reset: Register = set(0)
  def get: Int = value
  def inc: Register = set(value + 1)
  def dec: Register = set(value - 1)
  def copy(newValue: Int): Register = set(newValue)