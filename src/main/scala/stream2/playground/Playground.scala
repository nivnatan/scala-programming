package stream2.playground

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}

/**
  * Created by niv on 12/21/2020
  */
object Playground extends App {

  implicit val actorSystem = ActorSystem("Playground")
  implicit val materializer = ActorMaterializer()

  Source.single("hello, Streams!").to(Sink.foreach(println)).run()
}
