package pekkoservice

import org.apache.pekko
import scala.concurrent.Future
import scala.concurrent.Await
import pekko.util.Timeout
import concurrent.duration.DurationInt

object DigitsService:
  def provider: Iterator[String] = Connector.iterator


object Connector extends Iterable[String]:

  import pekko.actor.typed.{ActorRef, ActorSystem}
  import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
  import org.apache.pekko.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import scala.concurrent.ExecutionContext
  import scala.concurrent

  val system: ActorSystem[Communication.Command] = ActorSystem(DigitsProvider(), "rootsolver")

  given implicitSystem: ActorSystem[_] = system
  given timeout: Timeout = 5.seconds

  val nbProcs = Runtime.getRuntime.availableProcessors()
  system ! Communication.Start(nbProcs, 5, 10 ,"ugkcyxxp", system)

  import concurrent.ExecutionContext.Implicits.global

  def iterator = new Iterator[String]:
    override def hasNext: Boolean = true

    override def next(): String =
      val fromPekko: Future[Communication.UnitResult] = {
        system.ask(sender => Communication.Next(sender))
      }

      val result = fromPekko.collect:
        case UnitResult(Successful(_, value, _)) => value
        case _ => "error"

      Await.result(result, 5.seconds)
