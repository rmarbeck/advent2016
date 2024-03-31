package pekkoservice

import org.apache.pekko

import scala.concurrent.Future
import scala.concurrent.Await
import pekko.util.Timeout

import concurrent.duration.DurationInt

object DigitsService:
  private val BUFFER_MIN_SIZE = 5
  private val BUFFER_MAX_SIZE = 10
  private val BURST_SIZE = 5000

  import pekko.actor.typed.{ActorRef, ActorSystem}
  import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
  import org.apache.pekko.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import scala.concurrent.ExecutionContext
  import scala.concurrent
  val system: ActorSystem[Communication.Command] = ActorSystem(DigitsProvider(), "rootsolver")
  given implicitSystem: ActorSystem[_] = system
  given timeout: Timeout = 2.seconds
  val nbProcs = Runtime.getRuntime.availableProcessors()
  import concurrent.ExecutionContext.Implicits.global
  private var iteratorRunning = false

  private def stopPreviousBeforeStart(): Unit =
    if (iteratorRunning)
      system ! Communication.Stop()
    iteratorRunning = true

  def getIterator(root: String) = new Iterator[String]:
    stopPreviousBeforeStart()
    system ! Communication.Start(nbProcs, BUFFER_MIN_SIZE, BUFFER_MAX_SIZE , root, BURST_SIZE, system)

    override def hasNext: Boolean = true

    override def next(): String =
      val fromPekko: Future[Communication.UnitResult] = {
        system.ask(sender => Communication.Next(sender))
      }

      val result = fromPekko.collect:
        case UnitResult(value, index) => value

      Await.result(result, 2.seconds)