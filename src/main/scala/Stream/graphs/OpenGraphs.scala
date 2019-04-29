package Stream.graphs

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, FlowShape, SinkShape, SourceShape}
import akka.stream.scaladsl.{Broadcast, Concat, Flow, GraphDSL, Sink, Source}

/**
  * Created by niv on 19/04/2019.
  */
object OpenGraphs extends App {

  implicit val actorSystem = ActorSystem("OpenGraphs")
  implicit val materializer = ActorMaterializer()

  // create composite source that concatenates 2 sources
  // - emits ALL elements from the first source
  // - then ALL the elements from the second

  val firstSource = Source(1 to 10)
  val secondSource = Source(42 to 1000)

  val sourceGraph = Source.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      // step 2
      val concat = builder.add(Concat[Int](2))

      // step 3
      firstSource ~> concat
      secondSource ~> concat

      // step 4
      SourceShape(concat.out)
    }
  )

  sourceGraph.to(Sink.foreach(println))

  // complex sink

  val sink1 = Sink.foreach[Int](x => println(s"meaningful1 $x"))
  val sink2 = Sink.foreach[Int](x => println(s"meaningful2 $x"))

  val sinkGraph = Sink.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      // step 2
      val broadcast = builder.add(Broadcast[Int](2))

      // step 3
      broadcast ~> sink1
      broadcast ~> sink2

      // step 4
      SinkShape(broadcast.in)
    }
  )

  firstSource.to(sinkGraph).run()

  // flow that's composed of two other flows - one that adds 1 to a number, one that does number * 10

  val incrementer = Flow[Int].map(_ + 1)
  val multiplier = Flow[Int].map(_ * 10)

  val flowGraph = Flow.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      val incrementerShape = builder.add(incrementer)
      val multiplierShape = builder.add(multiplier)
      // step 2
      incrementerShape ~> multiplierShape

      // step 4
      FlowShape(multiplierShape.in, multiplierShape.out)
    }
  )
}
