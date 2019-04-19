package Stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}

/**
  * Created by niv on 19/04/2019.
  */
object Playground extends App {

  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()

  Source.single("hello stream!").to(Sink.foreach(println))
}
