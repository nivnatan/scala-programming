package stream.graphs

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Balance, Broadcast, Flow, GraphDSL, Merge, RunnableGraph, Sink, Source, Zip}
import akka.stream.{ActorMaterializer, ClosedShape}

/**
  * Created by niv on 19/04/2019.
  */
object Graphs extends App {

  implicit val actorSystem = ActorSystem("Graphs")
  implicit val materializer = ActorMaterializer()

  val input = Source(1 to 1000)
  val incrementer = Flow[Int].map(x => x + 1)
  val multiplier = Flow[Int].map(x => x * 10)
  val output = Sink.foreach[(Int,Int)](println)

  val graph = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
      import GraphDSL.Implicits._ // brings some nice operators into scope

      // step 2 - add the necessary components of this graph
      val broadcast = builder.add(Broadcast[Int](2)) // fan-out operator
      val zip = builder.add(Zip[Int, Int]) // fan-in operator

      // step 3 - tying up the components
      input ~> broadcast

      broadcast.out(0) ~> incrementer ~> zip.in0
      broadcast.out(1) ~> multiplier ~> zip.in1

      zip.out ~> output

      // step 4 - return a closed shape
      ClosedShape

      // shape
    } // graph
  ) // runnable graph

  // exercise 1: feed a source into 2 sinks at the same time (hint: use a broadcast)

  val graph2Sinks = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
      import GraphDSL.Implicits._ // brings some nice operators into scope

      val output1 = Sink.foreach[Int](println)
      val output2 = Sink.foreach[Int](println)

      // step 2 - add the necessary components of this graph
      val broadcast = builder.add(Broadcast[Int](2)) // fan-out operator

      // step 3 - tying up the components
      input ~> broadcast

      broadcast.out(0) ~> output1
      broadcast.out(1) ~> output2

      // another way to write it
      input ~> broadcast ~> output1 // implicit port numbering
               broadcast ~> output2

      // step 4 - return a closed shape
      ClosedShape

      // shape
    } // graph
  ) // runnable graph

  // exercise 2: balance

  import scala.concurrent.duration._
  val fastSource = input.throttle(5, 1 seconds)
  val slowSource = input.throttle(2, 1 seconds)
  val sink1 = Sink.foreach[Int](println)
  val sink2 = Sink.foreach[Int](println)

  val balanceGraph = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
      import GraphDSL.Implicits._ // brings some nice operators into scope

      // step 2 - declare components
      val merge = builder.add(Merge[Int](2))
      val balance = builder.add(Balance[Int](2))

      // step 3 - tie them up
      fastSource ~> merge ~> balance ~> sink1
      slowSource ~> merge
      balance ~> sink2

      ClosedShape

      // shape
    } // graph
  ) // runnable graph

  graph2Sinks.run() // run the graph and materialize it
}
