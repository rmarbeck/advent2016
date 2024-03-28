package pekkoservice

import org.apache.pekko
import pekko.actor.typed.ActorRef
import pekko.actor.typed.ActorSystem
import pekko.actor.typed.Behavior
import pekko.actor.typed.scaladsl.Behaviors
import pekko.actor.typed.scaladsl.ActorContext

object InternalMessages:
  sealed trait Computation
  final case class Mission(root: String, index: Int, sender: ActorRef[ComputationResult]) extends Computation
  final case class Finish() extends Computation
  final case class ResultFound(index: Int, result: String) extends Computation
  final case class NoResultFound(index: Int) extends Computation
  final case class Work() extends Computation

  sealed trait ComputationResult(val index: Int, val resolver: Option[ActorRef[Computation]])
  final case class Successful(indexSuccessful: Int, result: String, resolverSuccessful: Option[ActorRef[Computation]]) extends ComputationResult(indexSuccessful, resolverSuccessful)
  final case class NotMatching(indexNotMatching: Int, resolverNotMaching: Option[ActorRef[Computation]]) extends ComputationResult(indexNotMatching, resolverNotMaching)

export InternalMessages.*

object Communication:
  sealed trait Command
  final case class Start(maxWorkers: Int, maxBufferSize: Int, minBufferSize: Int, root: String, sender: ActorRef[Command]) extends Command
  final case class Pause() extends Command
  final case class Continue() extends Command
  final case class Stop() extends Command
  final case class Next(requester: ActorRef[UnitResult]) extends Command
  final case class UnitResult(result: ComputationResult) extends Command

export Communication.*

class DigitsBuffer(val min: Int, val max: Int):
  var data = Vector[(Int, String)]()
  def shouldFeed: Boolean = data.length < min
  def isFull: Boolean = data.length >= max
  def add(index: Int, value: String) = data = data. :+ (index, value)
  def hasNext: Boolean = data.nonEmpty
  def next: (Int, String) =
    val current = data.head
    data = data.tail
    current

object DigitsProvider:
  def apply(): Behavior[Command] =
    idle()

  private def idle(): Behavior[Command] =
    Behaviors.setup[Command] { context =>
      Behaviors.receiveMessage { message =>
        context.log.trace("[Idle] : Receiving message....... {}", message)

        message match
          case Start(maxChildren, maxBuffer, minBuffer, root, _) =>
            context.log.debug("[Idle] : Starting scheduler")
            val scheduler = context.spawn(DigitsScheduler(), s"digitsScheduler")
            scheduler ! Start(maxChildren, maxBuffer, minBuffer, root, context.self)
            context.log.debug("[Idle] : {} scheduler created")
            readyToResolve(scheduler, DigitsBuffer(minBuffer, maxBuffer))
          case UnitResult(_) =>
            context.log.debug("[Idle] : Receiving another answer, don't take it into account")
            Behaviors.same
          case message =>
            context.log.error("[Idle] : Unmanaged message received : {}", message.getClass)
            Behaviors.unhandled
      }
    }

  private def readyToResolve(scheduler: ActorRef[Command], buffer: DigitsBuffer, waitingList: Seq[ActorRef[UnitResult]] = Seq()): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      context.log.trace("[Ready] : Receiving message....... {}", message.getClass)

      given ActorContext[Command] = context

      message match
        case Next(sender) =>
          val updatedWaitingList = answerToFirstSender(buffer, waitingList, Some(sender))
          readyToResolve(scheduler, buffer, updatedWaitingList)
        case UnitResult(Successful(index, result, _)) =>
          context.log.info("[Ready] : Receiving answer, {}", result)
          buffer.add(index, result)
          val updatedWaitingList = answerToFirstSender(buffer, waitingList, None)
          if (!buffer.shouldFeed || !buffer.isFull)
            scheduler! Pause()
          else
            scheduler! Continue()
          readyToResolve(scheduler, buffer, updatedWaitingList)
        case Stop() =>
          context.log.debug("[Ready] : Stopping children")
          askToFinishAndStop()
          idle()
        case _ => Behaviors.unhandled
    }

  private def answerToFirstSender(buffer: DigitsBuffer, waitingList: Seq[ActorRef[UnitResult]], currentSender: Option[ActorRef[UnitResult]])(using context: ActorContext[Command]): Seq[ActorRef[UnitResult]] =
    if (buffer.hasNext)
      val (indexOfResult, valueOfResult) = buffer.next
      if (waitingList.nonEmpty || currentSender.nonEmpty)
        waitingList.headOption.getOrElse(currentSender.get) ! UnitResult(Successful(indexOfResult, valueOfResult, None))
        currentSender match
          case None => waitingList.tail
          case Some(value) if waitingList.isEmpty => Seq(value)
          case Some(value) => waitingList.tail :+ value
      else
        waitingList
    else
      waitingList

  private def askToFinishAndStop(): Unit = ()
