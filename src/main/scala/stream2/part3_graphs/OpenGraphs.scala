package stream2.part3_graphs

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Broadcast, Concat, Flow, GraphDSL, Keep, RunnableGraph, Sink, Source}
import akka.stream._

import scala.collection.parallel.immutable.HashSetCombiner

/**
  * Created by niv on 15/01/2020
  */
object OpenGraphs extends App {

  implicit val actorSystem = ActorSystem("OpenGraphs")
  implicit val materializer = ActorMaterializer()

  // recap
  // step1:
  val graph = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder =>

      // step 2: declaring componenets

      // step3: tying them together

      // step4
      ClosedShape
    }
  )

  /*
    A composite source that concatenates 2 sources:
      - emits all the elements from the first source
      - then all the elements from the second
  */

  val firstSource = Source(1 to 10)
  val secondSource = Source(42 to 1000)

  val sourceGraph = Source.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      // step 2: declaring components
      val concat = builder.add(Concat[Int](2))

      // step3: tying them together
      firstSource ~> concat
      secondSource ~> concat

      // step4
      SourceShape(concat.out)
    }
  )

  // we can do:
  //sourceGraph.to(Sink.foreach(println))

  /*
    complex sink
    we want to feed a source into both sinks
  */

  val sink1 = Sink.foreach[Int](x => println(s"sink1: $x"))
  val sink2 = Sink.foreach[Int](x => println(s"sink2: $x"))

  val sinkGraph = Sink.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      // step 2: declaring components
      val broadcast = builder.add(Broadcast[Int](2))

      // step3: tying them together
      broadcast ~> sink1
      broadcast ~> sink2

      // step4
      SinkShape(broadcast.in)
    }
  )

  //firstSource.runWith(sinkGraph)

  /*
    complex flow
    write your own flow that's composed of two other flows
      - one that adds 1 to a number
      - one that does number * 10
  */

  val flow1 = Flow[Int].map(x => x + 1)
  val flow2 = Flow[Int].map(x => x * 10)

  val flowGraph = Flow.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      // step 2: declaring components (define auxiliary shapes)
      // we need to transform the flows into shapes obj so we can tie them together
      val flowShape1 = builder.add(flow1)
      val flowShape2 = builder.add(flow2)

      // step3: tying them together (connect the shapes)
      flowShape1 ~> flowShape2

      // step4
      FlowShape(flowShape1.in, flowShape2.out)
    }
  )


  /*
     Exercise: flow from a sink and a source!
  */

 def fromSinkAndSource[A,B](sink: Sink[A, _], source: Source[B, _]): Flow[A, B, _] =
   Flow.fromGraph(
     GraphDSL.create() { implicit builder =>
       //step2: declare the shapes
       val sourceShape = builder.add(source)
       val sinkShape = builder.add(sink)

       //step3
       //step4 - return the shape
       FlowShape(sinkShape.in, sourceShape.out)
     }
   )

  val f = Flow.fromSinkAndSource(Sink.foreach[Int](x => println(s"here1 $x")), Source(100 to 102))
  // coupling
  val f2 = Flow.fromSinkAndSource(Sink.foreach[Int](println), Source(100 to 102))

  val s1 = Source(1 to 5)
  val s2 = Sink.foreach[Int](println)

  s1.viaMat(f2)(Keep.right).runWith(s2)
}
