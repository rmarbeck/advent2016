package pekkoservice

import org.apache.pekko
import pekko.actor.typed.ActorRef
import pekko.actor.typed.ActorSystem
import pekko.actor.typed.Behavior
import pekko.actor.typed.scaladsl.Behaviors
import pekko.actor.typed.scaladsl.ActorContext

object InternalMessages:
  sealed trait Computation
  final case class Mission(root: String, indexes: List[Int], sender: ActorRef[Computation]) extends Computation
  final case class Finish() extends Computation
  final case class ResultFound(forRoot: String, indexStart: Int, indexEnd: Int, found: List[(Int, String)], worker: ActorRef[Computation]) extends Computation
  final case class NoResultFound(forRoot: String, indexStart: Int, indexEnd: Int, worker: ActorRef[Computation]) extends Computation
  final case class Work() extends Computation

  sealed trait ComputationResult(val resolver: Option[ActorRef[Computation]])
  final case class Successful(resultFound: ResultFound) extends ComputationResult(Some(resultFound.worker))
  final case class NotMatching(resultNotFound: NoResultFound) extends ComputationResult(Some(resultNotFound.worker))

export InternalMessages.*

object Communication:
  sealed trait Command
  final case class Start(maxWorkers: Int, maxBufferSize: Int, minBufferSize: Int, root: String, poolSize: Int, sender: ActorRef[Command]) extends Command
  final case class Pause() extends Command
  final case class Continue() extends Command
  final case class Stop() extends Command
  final case class Next(requester: ActorRef[UnitResult]) extends Command
  final case class UnitResult(foundValue: String, index:Int) extends Command

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
    idle(None)

  private def idle(scheduler: Option[ActorRef[Command]]): Behavior[Command] =
    Behaviors.setup[Command] { context =>
      Behaviors.receiveMessage { message =>
        context.log.trace("[Idle] : Receiving message....... {}", message)

        message match
          case Start(maxChildren, maxBuffer, minBuffer, root, poolSize, _) =>
            val schedulerToUse = scheduler match
              case Some(value) => value
              case None =>
                context.log.debug("[Idle] : Starting scheduler")
                context.spawn(DigitsScheduler(), s"digitsScheduler")

            schedulerToUse ! Start(maxChildren, maxBuffer, minBuffer, root, poolSize, context.self)
            context.log.debug("[Idle] : {} scheduler created")
            readyToResolve(schedulerToUse, DigitsBuffer(minBuffer, maxBuffer))
          case UnitResult(_, _) =>
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
        case UnitResult(result, index) =>
          //println(s"Found : $result, $index, [buffer full ${buffer.isFull}]")
          context.log.debug("[Ready] : Receiving answer, {}", result)
          buffer.add(index, result)
          val updatedWaitingList = answerToFirstSender(buffer, waitingList, None)
          if (!buffer.shouldFeed || buffer.isFull)
            scheduler! Pause()
          else
            scheduler! Continue()
          readyToResolve(scheduler, buffer, updatedWaitingList)
        case Stop() =>
          context.log.info("[Ready] : Stopping children")
          askToFinishAndStop(scheduler)
          idle(Some(scheduler))
        case _ => Behaviors.unhandled
    }

  private def answerToFirstSender(buffer: DigitsBuffer, waitingList: Seq[ActorRef[UnitResult]], currentSender: Option[ActorRef[UnitResult]])(using context: ActorContext[Command]): Seq[ActorRef[UnitResult]] =
    def readySender: Option[ActorRef[UnitResult]] =
      waitingList match
        case Nil => currentSender
        case head :: tail => Some(head)

    def appendCurrentSender: Seq[ActorRef[UnitResult]] =
      currentSender match
        case Some(value) => waitingList :+ value
        case None => waitingList

    val newSenderList =
      readySender match
        case Some(sender) if buffer.hasNext =>
          val (indexOfResult, valueOfResult) = buffer.next
          sender ! UnitResult(valueOfResult, indexOfResult)
          appendCurrentSender.filterNot(_ == sender)
        case Some(sender) =>
          appendCurrentSender
        case _ => Nil

    newSenderList


  private def askToFinishAndStop(scheduler: ActorRef[Command]): Unit =
    scheduler ! Stop()
