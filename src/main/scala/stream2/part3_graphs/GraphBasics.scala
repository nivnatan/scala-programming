package stream2.part3_graphs

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape}
import akka.stream.scaladsl.RunnableGraph
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Sink, Source, Zip}

/**
  * Created by niv on 12/28/2020
  */
object GraphBasics extends App {

  implicit val actorSystem = ActorSystem("GraphBasics")
  implicit val materializer = ActorMaterializer()

  val input = Source(1 to 1000)
  // two hard computations
  val incrementer = Flow[Int].map(x => x + 1)
  val multiplier = Flow[Int].map(x => x * 10)
  // we would like to run the computations in parallel and tuple back the results
  val output = Sink.foreach[(Int, Int)](println)

  // step 1 - setting up the fundamentals for the graph
  val graph = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] => // builder = MUTABLE data structure
      import GraphDSL.Implicits._ // brings some nice operators into scope

      // step 2 - add the necessary componenets of this graph

      val broadcast = builder.add(Broadcast[Int](2)) // fan out operator - it has single input and two output
      val zip = builder.add(Zip[Int, Int]) // fan in operator - it has two inputs and one output

      // step 3 - tying up the components
      input ~> broadcast

      broadcast.out(0) ~> incrementer ~> zip.in0
      broadcast.out(1) ~> multiplier ~> zip.in1

      zip.out ~> output

      // step 4 - return a closed shape
      ClosedShape // the all process is mutating the builder and than freezing its state by making it immutable

      // we must return a shape
    } // graph
  ) // runnable graph

  //graph.run()

  // Ex: feed a source into 2 sinks at the same time(hint: use a broadcast)

  val graph2 = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>

      import GraphDSL.Implicits._

      val source = Source(1 to 1000)
      val sink1 = Sink.foreach[Int](x => println(s"Sink1: $x"))
      val sink2 = Sink.foreach[Int](x => println(s"Sink2: $x"))

      val broadcast = builder.add(Broadcast[Int](2))

      source ~> broadcast

      broadcast.out(0) ~> sink1
      broadcast.out(1) ~> sink2

      // this could also be written shortly as:

      source ~> broadcast ~> sink1
                broadcast ~> sink2

      ClosedShape
    }
  )




  graph2.run()
}
