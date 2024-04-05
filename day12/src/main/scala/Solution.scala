import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given registers: Registers = Registers()

    val instructions = inputLines.toArray.collect:
      case s"cpy ${input} ${to}" =>
        input.toIntOption match
          case Some(value) => CopyInt(value, to.asRegister)
          case None => CopyRegister(input.asRegister, to.asRegister)
      case s"inc ${x}" => Inc(x.asRegister)
      case s"dec ${x}" => Dec(x.asRegister)
      case s"jnz ${x} ${to}" =>
        x.toIntOption match
          case Some(value) => JNZInt(x.toInt, to.toInt)
          case None => JNZRegister(x.asRegister, to.toInt)

    val resultPart1 = process(instructions)

    registers.reset
    registers.byName("c").set(1)
    val resultPart2 = process(instructions)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def process(instructions: Array[Instruction], index: Int = 0)(using registers: Registers): Int =
  instructions.isDefinedAt(index) match
    case false => registers.byName("a").get
    case true =>
      instructions(index).apply() match
        case None => process(instructions, index + 1)
        case Some(value) => process(instructions, index + value)

extension (str: String)
  def asRegister(using Registers): Register = summon[Registers].byName(str)

class Registers:
  def reset: Unit = values.foreach((_, register) => register.reset)

  private val values: Map[String, Register] = Map("a" -> Register(), "b" -> Register(), "c" -> Register(), "d" -> Register())
  def byName(name: String): Register = values(name)

trait Instruction:
  def next: Option[Int] = None
  def update(): Unit = ()
  final def apply(): Option[Int] =
    update()
    next

case class CopyInt(x: Int, to: Register) extends Instruction:
  override def update(): Unit = to.copy(x)
case class CopyRegister(x: Register, to: Register) extends Instruction:
  override def update(): Unit = to.copy(x.get)
case class Inc(register: Register) extends Instruction:
  override def update(): Unit = register.inc
case class Dec(register: Register) extends Instruction:
  override def update(): Unit = register.dec
case class JNZInt(x: Int, to: Int) extends Instruction:
  override def next: Option[Int] =
    x match
      case 0 => None
      case _ => Some(to)
case class JNZRegister(x: Register, to: Int) extends Instruction:
  override def next: Option[Int] = JNZInt(x.get, to).next


class Register:
  private var value = 0
  def set(newValue: Int): Register =
    value = newValue
    this
  def reset: Register = set(0)
  def get: Int = value
  def inc: Register = set(value + 1)
  def dec: Register = set(value - 1)
  def copy(newValue: Int): Register = set(newValue)
