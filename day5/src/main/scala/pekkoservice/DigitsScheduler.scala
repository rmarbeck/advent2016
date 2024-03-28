package pekkoservice

import org.apache.pekko
import pekko.actor.typed.ActorRef
import pekko.actor.typed.ActorSystem
import pekko.actor.typed.Behavior
import pekko.actor.typed.scaladsl.Behaviors
import pekko.actor.typed.scaladsl.ActorContext

object DigitsScheduler:
  def apply(): Behavior[Command] =
    idle()

  private def idle(): Behavior[Command] =
    Behaviors.setup[Command] { context =>
      Behaviors.receiveMessage { message =>
        context.log.trace("[Idle] : Receiving message....... {}", message)

        message match
          case Start(maxChildren, _, _, root, provider) =>
            context.log.debug("[Idle] : Starting {} children", maxChildren)
            val children =
              for i <- 1 to maxChildren
              yield
                context.spawn(DigitsWorker(i), s"worker$i")

            context.log.debug("[Idle] : {} children created", children.length)
            context.self ! Continue()
            find(WorkingStatus(children, Seq(), PendingResults(), true, root, 0, provider))
          case UnitResult(_) =>
            context.log.debug("[Idle] : Receiving another answer, don't take it into account")
            Behaviors.same
          case message =>
            context.log.error("[Idle] : Unmanaged message received : {}", message.getClass)
            Behaviors.unhandled
      }
    }

  private def find(workingStatus: WorkingStatus): Behavior[Command] =
    val WorkingStatus(availableChildren, workingChildren, results, _, root, currentIndex, _) = workingStatus
    Behaviors.receive[Command] { (context, message) =>
      context.log.trace("[Ready] : Receiving message....... {}", message.getClass)

      given ActorContext[Command] = context

      message match
        case Pause() =>
          find(workingStatus.copy(shouldContinue = false))
        case Continue() =>
          val backendResponseMapper: ActorRef[ComputationResult] =
            context.messageAdapter(rsp => UnitResult(rsp))
          availableChildren.zipWithIndex.foreach:
            (child, idx) => child ! Mission(root, currentIndex + idx, backendResponseMapper)
            results.addWaiting(idx)

          find {
            workingStatus.copy(
              availableChildren = Seq(),
              workingChildren = availableChildren ++ workingChildren,
              shouldContinue = true,
              currentIndex = currentIndex + availableChildren.length
            )
          }
        case UnitResult(Successful(index, result, Some(child))) =>
          results.markSuccessful(index, result)
          sendAvailableResult(workingStatus, child)

          find {
            workingStatus.copy(
              availableChildren = availableChildren :+ child,
              workingChildren = workingChildren.filterNot(_ == child),
            )
          }
        case UnitResult(NotMatching(index, Some(child))) =>
          results.markNotFound(index)
          sendAvailableResult(workingStatus, child)

          find {
            workingStatus.copy(
              availableChildren = availableChildren :+ child,
              workingChildren = workingChildren.filterNot(_ == child),
            )
          }
        case Stop() =>
          context.log.debug("[Ready] : Stopping children")
          askChildrenToFinishAndStop(availableChildren ++ workingChildren)
          idle()
        case _ => Behaviors.unhandled
    }

  private def sendAvailableResult(workingStatus: WorkingStatus, child: ActorRef[Computation])(using context: ActorContext[Command]): Unit =
    workingStatus.results.shrink().foreach:
      readyResult => workingStatus.provider ! UnitResult(Successful(readyResult._1, readyResult._2, Some(child)))

    if (workingStatus.shouldContinue)
      context.self ! Continue()

  private def askChildrenToFinishAndStop(children: Seq[ActorRef[Computation]]) = ()

case class WorkingStatus(availableChildren: Seq[ActorRef[Computation]], workingChildren: Seq[ActorRef[Computation]], results: PendingResults, shouldContinue: Boolean, root: String, currentIndex: Int, provider: ActorRef[Command])

class PendingResults:
  val data = scala.collection.mutable.Map[Int, Either[Boolean, String]]()

  def addWaiting(index: Int): Unit = data.update(index, Left(false))
  private def removeFromHead(): Unit =
    data.toSeq.sortBy(_._1).span(_._2 == Left(true))._1.foreach:
      (key, _) => data.remove(key)
  def shrink(): Option[(Int, String)] =
    removeFromHead()
    val sortedData = data.toSeq.sortBy(_._1)
    val firstValue =
      sortedData.find:
        case (_, Right(value)) => true
        case _ => false
    val result = firstValue match
      case Some((indexFound, Right(valueFound))) =>
        sortedData.filter(cur => cur._1 < indexFound).forall(current => current._2 == Left(true)) match
          case true => Some((indexFound, valueFound))
          case false => None
      case _ => None

    result

  def markSuccessful(index: Int, value: String) = data.update(index, Right(value))
  def markNotFound(index: Int) = data.update(index, Left(true))
