package stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by niv on 19/04/2019.
  */
object Playground extends App {

  ""

  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()

  Source.single("hello stream!").to(Sink.foreach(println))

  val res = Future {
    5
  }.filter(_ > 3)

  res.onComplete(result => println(result))
}
