package stream

import akka.actor.{Actor, ActorSystem, Props}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}

/**
  * Created by niv on 20/04/2019.
  */
object OperatorFusion {

  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()
  import actorSystem.dispatcher

  val simpleSource = Source(1 to 1000)
  val simpleFlow = Flow[Int].map(_ + 1)
  val simpleFlow2 = Flow[Int].map(_ * 10)
  val simpleSink = Sink.foreach[Int](println)

  // this runs on the SAME ACTOR - operator/component FUSION
  simpleSource.via(simpleFlow).via(simpleFlow2).to(simpleSink).run()

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case x: Int =>
        // flow operations
        val x2 = x + 1
        val y = x2 * 10
        // sink operation
        println(y)
    }
  }
  val simpleActor = actorSystem.actorOf(Props[SimpleActor])
  (1 to 1000).foreach(simpleActor ! _)
}
