package pekkoservice

import org.apache.pekko
import pekko.actor.typed.{Behavior, ActorRef}
import pekko.actor.typed.scaladsl.{Behaviors, ActorContext}

object DigitsScheduler:
  def apply(): Behavior[Command | ComputationResult] =
    idle(None)

  private def idle(children: Option[Seq[ActorRef[Computation]]]): Behavior[Command | ComputationResult] =
    Behaviors.setup[Command | ComputationResult] { context =>
      Behaviors.receiveMessage {
        case Start(maxChildren, _, _, root, poolSize, provider) =>
          val childrenToSendWorkTo = children match
            case Some(value) => value
            case None =>
              val children =
                for i <- 1 to maxChildren
                  yield
                    context.spawn(DigitsWorker(i), s"worker$i")
              children

          context.self ! Continue()
          find(WorkingStatus(childrenToSendWorkTo, Seq(), AggregatedResults(), true, root, 0, poolSize, provider))
        case UnitResult(_, _) =>
          context.log.debug("[Idle] : Receiving another answer, don't take it into account")
          Behaviors.same
        case NotMatching(_) =>
          context.log.debug("[Idle] : Receiving not matching answer, don't take it into account")
          Behaviors.same
        case message =>
          context.log.error("[Idle] : Unmanaged message received : {}", message.getClass)
          Behaviors.unhandled
      }
    }

  private def find(workingStatus: WorkingStatus): Behavior[Command | ComputationResult] =
    val WorkingStatus(availableChildren, workingChildren, results, _, root, currentIndex, poolSize, _) = workingStatus
    Behaviors.receive[Command | ComputationResult] { (context, message) =>

      given ActorContext[Command | ComputationResult] = context

      message match
        case Pause() =>
          find(workingStatus.copy(shouldContinue = false))
        case Continue() =>
          val backendResponseMapper: ActorRef[Computation] =
            context.messageAdapter:
              case found: ResultFound => Successful(found)
              case notFound: NoResultFound => NotMatching(notFound)
              case _ => throw Exception("Not managed")

          availableChildren.zipWithIndex.foreach:
            (child, idx) =>
              val (indexMin, indexMax) = (currentIndex + idx*poolSize, currentIndex + (idx+1)*poolSize - 1)
              child ! Mission(root, indexMin, indexMax , backendResponseMapper)

          find {
            workingStatus.copy(
              availableChildren = Seq(),
              workingChildren = availableChildren ++ workingChildren,
              shouldContinue = true,
              currentIndex = currentIndex + availableChildren.length * poolSize
            )
          }
        case Successful(ResultFound(root, indexStart, indexEnd, workerResult, child)) if root == workingStatus.root =>
          workerResult.sortBy(_._1).foreach:
            (index, result) => results.markSuccessful(index, result)

          manageResultAndSend(workingStatus, indexStart, indexEnd, child)

        case NotMatching(NoResultFound(root, indexStart, indexEnd, child)) if root == workingStatus.root =>
          manageResultAndSend(workingStatus, indexStart, indexEnd, child)

        case NotMatching(_) | Successful(_) =>
          context.log.info("[Ready] : Receiving result of a previous mission")
          Behaviors.same
        case Stop() =>
          context.log.debug("[Ready] : Stopping children")
          askChildrenToFinishAndStop(availableChildren ++ workingChildren)
          idle(Some(availableChildren ++ workingChildren))
        case _ => Behaviors.unhandled
    }

  private def manageResultAndSend(workingStatus: WorkingStatus, indexStart: Int, indexEnd: Int, child: ActorRef[Computation])(using ActorContext[Command | ComputationResult]) : Behavior[Command | ComputationResult] =
    workingStatus.results.markSolved(indexStart, indexEnd)

    sendAvailableResult(workingStatus, child)

    find {
      workingStatus.copy(
        availableChildren = workingStatus.availableChildren :+ child,
        workingChildren = workingStatus.workingChildren.filterNot(_ == child),
      )
    }


  private def sendAvailableResult(workingStatus: WorkingStatus, child: ActorRef[Computation])(using context: ActorContext[Command | ComputationResult]): Unit =
    workingStatus.results.shrink().foreach:
      readyResult =>  workingStatus.provider ! UnitResult(readyResult._2, readyResult._1)

    if (workingStatus.shouldContinue)
      context.self ! Continue()

  private def askChildrenToFinishAndStop(children: Seq[ActorRef[Computation]]): Unit =
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
    private def merge(newRange: MinMax, inRanges: List[MinMax]): MinMax =
      inRanges.fold(newRange):
        case (acc, (currentMin, currentMax)) if currentMin - 1 == acc._2 => (acc._1, currentMax)
        case (acc, (currentMin, currentMax)) if currentMax + 1 == acc._1 => (currentMin, acc._2)
        case _ => throw Exception("Not managed")

    private var sortedRanges: List[MinMax] = List((min, min))
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