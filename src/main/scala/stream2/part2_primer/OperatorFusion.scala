package stream2.part2_primer

import akka.actor.{Actor, ActorSystem, Props}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}

/**
  * Created by niv on 26/12/2020
  */
object OperatorFusion extends App {

  implicit val actorSystem = ActorSystem("OperatorFusion")
  implicit val materializer = ActorMaterializer()

  val simpleSource = Source(1 to 1000)
  val simpleFlow = Flow[Int].map(_ + 1)
  val simpleFlow2 = Flow[Int].map(_ * 10)
  val simpleSink = Sink.foreach[Int](println)

  // this runs on the same actor
  simpleSource.via(simpleFlow).via(simpleFlow2).to(simpleSink).run()
  // this is called operator/component FUSION

  // "equivalent"
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

  // complex flows:
  val complexFlow = Flow[Int].map { x =>
    // simulating a long computation
    Thread.sleep(1000)
    x + 1
  }

  val complexFlow2 = Flow[Int].map { x =>
    // simulating a long computation
    Thread.sleep(1000)
    x * 10
  }

  // opearting on the same actor ! 2 seconds between each println
  simpleSource.via(complexFlow).via(complexFlow2).to(simpleSink).run()

  // async boundary
  simpleSource.via(complexFlow).async // runs on one actor
    .via(complexFlow2).async // runs on another actor
    .to(simpleSink) // run on a third actor
    .run()

  // ordering guarantees
  Source(1 to 3)
    .map(element => { println(s"element A: $element"); element })
    .map(element => { println(s"element B: $element"); element })
    .map(element => { println(s"element C: $element"); element })
    .runWith(Sink.ignore)

  //A->1
  //B->1
  //C->1
  //..
  //A->3
  //B->3
  //C->3

  Source(1 to 3)
    .map(element => { println(s"element A: $element"); element }).async
    .map(element => { println(s"element B: $element"); element }).async
    .map(element => { println(s"element C: $element"); element }).async
    .runWith(Sink.ignore)

  // the relative order for each element will be kept, but different elements could be processed simultaneously
}
