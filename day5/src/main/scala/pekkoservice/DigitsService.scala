package pekkoservice

import org.apache.pekko
import scala.concurrent.Future
import scala.concurrent.Await
import pekko.util.Timeout
import concurrent.duration.DurationInt

object DigitsService:
  def provider(key: String): Iterator[String] =
    Connector.changeRoot(key)
    Connector.iterator
  def stop: Unit = Connector.stop


object Connector extends Iterable[String]:
  var currentRoot: String = ""
  def changeRoot(str: String) =
    currentRoot = str

  import pekko.actor.typed.{ActorRef, ActorSystem}
  import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
  import org.apache.pekko.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import scala.concurrent.ExecutionContext
  import scala.concurrent

  val system: ActorSystem[Communication.Command] = ActorSystem(DigitsProvider(), "rootsolver")

  given implicitSystem: ActorSystem[_] = system
  given timeout: Timeout = 20.seconds

  val nbProcs = Runtime.getRuntime.availableProcessors()


  import concurrent.ExecutionContext.Implicits.global

  def stop = system ! Communication.Stop()

  def iterator = new Iterator[String]:
    system ! Communication.Start(nbProcs, 5, 10 , currentRoot, 2500, system)
    override def hasNext: Boolean = true

    override def next(): String =
      val fromPekko: Future[Communication.UnitResult] = {
        system.ask(sender => Communication.Next(sender))
      }

      val result = fromPekko.collect:
        case UnitResult(value, index) => value

      Await.result(result, 20.seconds)
