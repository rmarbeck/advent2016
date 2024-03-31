package pekkoservice

import org.apache.pekko
import pekko.actor.typed.{Behavior, ActorRef, PostStop, DispatcherSelector}
import pekko.actor.typed.scaladsl.{Behaviors, ActorContext}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success

import scala.annotation.tailrec

object DigitsWorker:

  def apply(workerId: Int): Behavior[Computation] =
    manage(workerId)

  private def manage(workerID: Int): Behavior[Computation] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage[Computation] {
        case Mission(root, indexMin, indexMax, sender) =>
          context.log.debug(s"Computer[$workerID] Starting mission {} with indexes {} <-> {}", root, indexMin, indexMax)
          context.self ! Work()
          work(root, indexMin, indexMax, sender, workerID)
        case Finish() =>
          context.log.info("[Idle] : No need to finish, not working.......")
          Behaviors.same
        case Work() =>
          context.log.debug("[Idle] : Receiving continue, don't take it into account")
          Behaviors.same
        case value =>
          context.log.error("[Idle] : Unmanaged message received : {}", value.getClass)
          Behaviors.unhandled
      }.receiveSignal {
        case (context, PostStop) =>
          context.log.error("Computation stopped")
          Behaviors.same
        case other =>
          context.log.error(s"Receiving a signal $other")
          Behaviors.same
      }
    }

  private def work(root: String, indexMin: Int, indexMax: Int, requester: ActorRef[Computation], workerID: Int): Behavior[Computation] =
    Behaviors.receive[Computation] { (context, message) =>

      message match
        case Work() =>
          given executionContext: ExecutionContext =
            context.system.dispatchers.lookup(DispatcherSelector.fromConfig("for-blocking-dispatcher"))

          doRunMission(root, indexMin, indexMax, workerID).andThen:
            case Success(list: List[(Int, Option[String])]) =>
              list.flatMap(_._2).length match
                case 0 => requester ! NoResultFound(root, indexMin, indexMax, context.self)
                case _ =>
                  val results = list.collect:
                    case (index, Some(str)) => (index, str)
                  requester ! ResultFound(root, indexMin, indexMax, results, context.self)
            case _ =>
              context.log.error("[work] : Error in retry, finishing")
              context.self ! Finish()

          manage(workerID)

        case Finish() =>
          context.log.info("[work] : Finishing.......")
          manage(workerID)

        case _ =>
          context.log.error("[work] : Unmanaged message received : {}", message.getClass)
          Behaviors.unhandled
    }

  private def doRunMission(root: String, indexMin: Int, indexMax: Int, workerId: Int)(using executionContext: ExecutionContext): Future[List[(Int, Option[String])]] =
    @tailrec
    def search(currentIndex: Int, indexMax: Int, previousResults: List[(Int, Option[String])] = Nil): List[(Int, Option[String])] =
      import solution.*
      currentIndex >= indexMax  match
        case true => previousResults
        case false =>
          val currentResult = MD5.firstCharAfter5ZerosInHash(s"$root$currentIndex") match
            case Some(value) => (currentIndex, Some(value))
            case None => (currentIndex, None)
          search(currentIndex+1, indexMax, currentResult +: previousResults)

    Future {
      search(indexMin, indexMax)
    }