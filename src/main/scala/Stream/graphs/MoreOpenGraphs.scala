package Stream.graphs

import akka.actor.ActorSystem
import akka.stream.javadsl.RunnableGraph
import akka.stream.{ActorMaterializer, ClosedShape, UniformFanInShape}
import akka.stream.scaladsl.{GraphDSL, Sink, Source, ZipWith}

/**
  * Created by niv on 01/05/2019.
  */
object MoreOpenGraphs extends App {

  implicit val actorSystem = ActorSystem("MoreOpenGraphs")
  implicit val materializer = ActorMaterializer()

  // example: Max3 operator
  // - 3 inputs of type int
  // - the maximum of the 3

  val max3StaticGraph = GraphDSL.create() { implicit builder =>
    import GraphDSL.Implicits._

    // step 2 - define aux shapes
    val max1 = builder.add(ZipWith[Int, Int, Int]((a,b) => Math.max(a,b)))
    val max2 = builder.add(ZipWith[Int, Int, Int]((a,b) => Math.max(a,b)))

    // step 3
    max1.out ~> max2.in0

    // step 4

    UniformFanInShape(max2.out, max1.in0, max1.in1, max2.in1)
  }

  val source1 = Source(1 to 10)
  val source2 = Source(1 to 10).map(_ => 5)
  val source3 = Source((1 to 10).reverse)

  val maxSink = Sink.foreach[Int](x => println(s"Max is: $x"))

  val max3RunnableGraph = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      // step 2 - declare SHAPES
      val max3Shape = builder.add(max3StaticGraph)

      // step 3 - tie
      source1 ~> max3Shape.in(0)
      source2 ~> max3Shape.in(1)
      source3 ~> max3Shape.in(2)

      max3Shape.out ~> maxSink

      // step 4
      ClosedShape
    }
  )

  max3RunnableGraph.run(materializer)
}
