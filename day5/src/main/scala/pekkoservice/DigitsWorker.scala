package pekkoservice

import org.apache.pekko
import pekko.actor.typed.{Behavior, ActorRef, ActorSystem, PostStop, DispatcherSelector}
import pekko.actor.typed.scaladsl.{Behaviors, ActorContext}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success

import InternalMessages.*

object DigitsWorker:

  def apply(index: Int): Behavior[Computation] =
    manage(index)

  private def manage(index: Int): Behavior[Computation] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage[Computation] {
        case Mission(root, indexTo, sender) =>
          context.log.debug(s"Computer[$index] Starting mission {} with index {}", root, indexTo)
          context.self ! Work()
          work(root, indexTo, sender, index)
        case Finish() =>
          context.log.debug("[Idle] : No need to finish, not working.......")
          Behaviors.same
        case Work() =>
          context.log.debug("[Idle] : Receiving continue, don't take it into account")
          Behaviors.same
        case ResultFound(_, _) =>
          context.log.debug("[Idle] : Receiving good result, don't take it into account")
          Behaviors.same
        case value =>
          context.log.error("[Idle] : Unmanaged message received : {}", value.getClass)
          Behaviors.unhandled
      }.receiveSignal {
        case (context, PostStop) =>
          context.log.debug("Computation stopped")
          Behaviors.same
        case other =>
          context.log.debug(s"Receiving a signal $other")
          Behaviors.same
      }
    }

  private def work(root: String, indexTo: Int, requester: ActorRef[ComputationResult], index: Int): Behavior[Computation] =
    Behaviors.receive[Computation] { (context, message) =>
      context.log.trace("[work] : Receiving message....... {}", message)

      message match
        case Work() =>
          context.log.debug("[work] : Receiving Work()")

          given executionContext: ExecutionContext =
            context.system.dispatchers.lookup(DispatcherSelector.fromConfig("for-blocking-dispatcher"))

          doRunMission(root, indexTo, index).andThen:
            case Success(Some((result: String, atIndex: Int))) =>
              requester ! Successful(indexTo, result, Some(context.self))
            case Success(None) =>
              requester ! NotMatching(indexTo, Some(context.self))
            case _ =>
              context.log.error("[work] : Error in retry, finishing")
              context.self ! Finish()

          manage(index)

        case Finish() =>
          context.log.debug("[work] : Finishing.......")
          manage(index)

        case _ =>
          context.log.error("[work] : Unmanaged message received : {}", message.getClass)
          Behaviors.unhandled
    }

  private def doRunMission(root: String, indexTo: Int, index: Int)(using executionContext: ExecutionContext): Future[Option[(String, Int)]] =
    println(s"$root$indexTo ($index)")
    import solution.*
    Future {
      MD5.firstCharAfter5ZerosInHash(s"$root$indexTo") match
        case Some(value) => Some(value, indexTo)
        case None => None
    }