package pekkoservice

import org.apache.pekko
import pekko.actor.typed.{Behavior, ActorRef, ActorSystem, PostStop, DispatcherSelector}
import pekko.actor.typed.scaladsl.{Behaviors, ActorContext}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success

object DigitsScheduler:
  def apply(): Behavior[Command | ComputationResult] =
    idle(None)

  private def idle(children: Option[Seq[ActorRef[Computation]]]): Behavior[Command | ComputationResult] =
    Behaviors.setup[Command | ComputationResult] { context =>
      Behaviors.receiveMessage { message =>
        context.log.trace("[Idle] : Receiving message....... {}", message)

        message match
          case Start(maxChildren, _, _, root, poolSize, provider) =>
            val childrenToUse = children match
              case Some(value) => value
              case None =>
                context.log.debug("[Idle] : Starting {} children", maxChildren)
                val children =
                  for i <- 1 to maxChildren
                  yield
                    context.spawn(DigitsWorker(i), s"worker$i")
                children

            context.self ! Continue()
            find(WorkingStatus(childrenToUse, Seq(), AggregatedResults(), true, root, 0, poolSize, provider))
          case UnitResult(_, _) =>
            context.log.debug("[Idle] : Receiving another answer, don't take it into account")
            Behaviors.same
          case message =>
            context.log.info("[Idle] : Unmanaged message received : {}", message.getClass)
            Behaviors.unhandled
      }
    }

  private def find(workingStatus: WorkingStatus): Behavior[Command | ComputationResult] =
    val WorkingStatus(availableChildren, workingChildren, results, _, root, currentIndex, poolSize, _) = workingStatus
    Behaviors.receive[Command | ComputationResult] { (context, message) =>
      context.log.trace("[Ready] : Receiving message....... {}", message.getClass)

      given ActorContext[Command | ComputationResult] = context

      message match
        case Pause() =>
          println("PAUSING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

          find(workingStatus.copy(shouldContinue = false))
        case Continue() =>
          val backendResponseMapper: ActorRef[Computation] =
            context.messageAdapter:
              case found: ResultFound =>
                //println("Result found sent")
                Successful(found)
              case notFound: NoResultFound => NotMatching(notFound)
              case _ => throw Exception("Not managed")
          availableChildren.zipWithIndex.foreach:
            (child, idx) =>
              val range = (currentIndex + idx*poolSize) until (currentIndex + (idx+1)*poolSize)
              child ! Mission(root, range.toList , backendResponseMapper)

          find {
            workingStatus.copy(
              availableChildren = Seq(),
              workingChildren = availableChildren ++ workingChildren,
              shouldContinue = true,
              currentIndex = currentIndex + availableChildren.length * poolSize
            )
          }
        case Successful(ResultFound(root, indexStart, indexEnd, workerResult, child)) if root == workingStatus.root =>
          //println(s"Managing found result ${results}")
          workerResult.sortBy(_._1).foreach:
            (index, result) => results.markSuccessful(index, result)

          results.markSolved(indexStart, indexEnd)

          //println(s"Managed found result ${results}")
          sendAvailableResult(workingStatus, child)

          find {
            workingStatus.copy(
              availableChildren = availableChildren :+ child,
              workingChildren = workingChildren.filterNot(_ == child),
            )
          }
        case NotMatching(NoResultFound(root, indexStart, indexEnd, child)) if root == workingStatus.root =>
          results.markSolved(indexStart, indexEnd)

          sendAvailableResult(workingStatus, child)

          find {
            workingStatus.copy(
              availableChildren = availableChildren :+ child,
              workingChildren = workingChildren.filterNot(_ == child),
            )
          }
        case NotMatching(_) | Successful(_) =>
          context.log.error("[Ready] : Receiving result of a previous mission")
          Behaviors.same
        case Stop() =>
          context.log.debug("[Ready] : Stopping children")
          askChildrenToFinishAndStop(availableChildren ++ workingChildren)
          idle(Some(availableChildren ++ workingChildren))
        case _ => Behaviors.unhandled
    }

  private def sendAvailableResult(workingStatus: WorkingStatus, child: ActorRef[Computation])(using context: ActorContext[Command | ComputationResult]): Unit =
    workingStatus.results.shrink().foreach:
      readyResult =>  workingStatus.provider ! UnitResult(readyResult._2, readyResult._1)

    if (workingStatus.shouldContinue)
      context.self ! Continue()

  private def askChildrenToFinishAndStop(children: Seq[ActorRef[Computation]]) =
    children.foreach:
      _ ! Finish()

case class WorkingStatus(availableChildren: Seq[ActorRef[Computation]], workingChildren: Seq[ActorRef[Computation]], results: AggregatedResults, shouldContinue: Boolean, root: String, currentIndex: Int, poolSize: Int, provider: ActorRef[Command])


type MinMax = (Int, Int)

class AggregatedResults:
  override def toString: String = s"AggregatedResults: ${validResults.size} in ${rangesStore} "
  class RangesStore(min: Int):
    override def toString: String = s"RangeStore: ${sortedRanges.size}"
    private def compact(sorted: List[MinMax]): List[MinMax] =
      sorted match
        case Nil => Nil
        case head :: tail =>
          if (head._2 != head._1)
            (head._2, head._2) :: tail
          else
            sorted
    def merge(newRange: MinMax, inRanges: List[MinMax]): MinMax =
      inRanges.fold(newRange):
        case (acc, (currentMin, currentMax)) if currentMin - 1 == acc._2 => (acc._1, currentMax)
        case (acc, (currentMin, currentMax)) if currentMax + 1 == acc._1 => (currentMin, acc._2)
        case _ => throw Exception("Not managed")

    var sortedRanges: List[MinMax] = List((min, min))
    def addRange(min: Int, max: Int): RangesStore =
      val (connected, notConnected) = sortedRanges.partition:
        case (currentMin, currentMax) if currentMin - 1 == max || currentMax + 1 == min => true
        case _ => false

      val updated = merge((min, max), connected)
      sortedRanges = compact((updated +: notConnected).sortBy(_._1))
      this

    def hasValueBefore(index: Int): Boolean = sortedRanges.head._2 >= index

  private var validResults: List[(Int, String)] = List()
  private val rangesStore: RangesStore = RangesStore(-1)


  def markSuccessful(index: Int, value: String): Unit =
    validResults = (validResults :+ (index, value)).sortBy(_._1)

  def markSolved(indexStart: Int, indexEnd: Int) =
    rangesStore.addRange(indexStart, indexEnd)

  def shrink(): List[(Int, String)] =
    validResults match
      case Nil => List()
      case head :: tail =>
        val (result, remaining) = validResults.span(res => rangesStore.hasValueBefore(res._1))
        validResults = remaining
        result