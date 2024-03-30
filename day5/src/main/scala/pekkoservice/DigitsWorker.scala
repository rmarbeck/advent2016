package pekkoservice

import org.apache.pekko
import pekko.actor.typed.{Behavior, ActorRef, ActorSystem, PostStop, DispatcherSelector}
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
        case Mission(root, indexesTo, sender) =>
          context.log.debug(s"Computer[$workerID] Starting mission {} with index {}", root, indexesTo)
          context.self ! Work()
          work(root, indexesTo, sender, workerID)
        case Finish() =>
          context.log.info("[Idle] : No need to finish, not working.......")
          Behaviors.same
        case Work() =>
          context.log.debug("[Idle] : Receiving continue, don't take it into account")
          Behaviors.same
        case ResultFound(_, _, _, _, _) =>
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

  private def work(root: String, indexesTo: List[Int], requester: ActorRef[Computation], workerID: Int): Behavior[Computation] =
    Behaviors.receive[Computation] { (context, message) =>
      context.log.trace("[work] : Receiving message....... {}", message)

      message match
        case Work() =>
          context.log.debug("[work] : Receiving Work()")

          given executionContext: ExecutionContext =
            context.system.dispatchers.lookup(DispatcherSelector.fromConfig("for-blocking-dispatcher"))

          doRunMission(root, indexesTo, workerID).andThen:
            case Success(list: List[(Int, Option[String])]) =>
              //println(s"[$workerID] until ${indexesTo.last}")
              list.map(_._2).flatten.length match
                case 0 => requester ! NoResultFound(root, indexesTo.head, indexesTo.last, context.self)
                case _ =>
                  val results = list.collect:
                    case (index, Some(str)) =>
                      //println(s"worker : $str => $index")
                      (index, str)
                  requester ! ResultFound(root, indexesTo.head, indexesTo.last, results, context.self)
            case _ =>
              context.log.error("[work] : Error in retry, finishing")
              context.self ! Finish()

          manage(workerID)

        case Finish() =>
          context.log.error("[work] : Finishing.......")
          manage(workerID)

        case _ =>
          context.log.error("[work] : Unmanaged message received : {}", message.getClass)
          Behaviors.unhandled
    }

  private def doRunMission(root: String, indexesTo: List[Int], workerId: Int)(using executionContext: ExecutionContext): Future[List[(Int, Option[String])]] =
    @tailrec
    def search(remainingIndexes: List[Int], previousResults: List[(Int, Option[String])] = Nil): List[(Int, Option[String])] =
      import solution.*
      remainingIndexes match
        case Nil => previousResults
        case head :: tail =>
          val currentResult = MD5.firstCharAfter5ZerosInHash(s"$root$head") match
            case Some(value) => (head, Some(value))
            case None => (head, None)
          search(tail, currentResult +: previousResults)

    Future {
      search(indexesTo)
    }