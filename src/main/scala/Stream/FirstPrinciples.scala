package Stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}

import scala.concurrent.Future

/**
  * Created by niv on 19/04/2019.
  */
object FirstPrinciples extends App {

  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()

  // sources
  val source = Source(1 to 10)
  // sinks
  val sink = Sink.foreach[Int](println)

  val graph = source.to(sink)
  graph.run()

  // flows transform elements
  val flow = Flow[Int].map(x => x + 1)

  val sourceWithFlow = source.via(flow)
  val flowWithSink = flow.to(sink)

  sourceWithFlow.to(sink).run()
  source.to(flowWithSink).run()
  source.via(flow).to(sink).run()

  // nulls are not allowed
  //val illegalSource = Source.single[String](null)
  //illegalSource.to(Sink.foreach(println)).run() // ERROR

  // various kinds of sources
  val finiteSource = Source.single(1)
  val anotherFiniteSource = Source(List(1,2,3))
  val emptySource = Source.empty[Int]
  //val inifiniteSource = Source(akka.stream.Stream.from(1))

  // sinks
  val theMostBoringSink = Sink.ignore
  val foreachSink = Sink.foreach[String](println)
  val headSink = Sink.head[Int] // retries head and then closes the stream
  val foldSink = Sink.fold[Int,Int](0)((a,b) => a + b)

  // flows - usually mapped to collection operators
  val mapFlow = Flow[Int].map(x => 2 + x)
  val takeFlow = Flow[Int].take(5)
  // drop, filter
  // Not gave flatMap

  // source -> flow -> flow -> ... -> sink
  val doubleFlowGraph = source.via(mapFlow).via(takeFlow).to(sink)
  doubleFlowGraph.run()

  // syntactic sugar
  val mapSource = Source(1 to 10).map(x => x + 2) // Source(1 to 30).via(Flow[Int].map(x => x + 2))
  // run streams directly
  mapSource.runForeach(println) // mapSource.to(Sink.foreach[Int](println).run()

  // OPERATORS = components

  /**
    * Exercise: create a stream that takes the names of persons, then you will keep the first 2 names with length > 5 characters
    * print to console
    */

  Source(List("bob", "rooobi", "tankkk", "moshhhhje", "moshhhhhe2"))
    .filter(_.length > 5)
    .take(2)
    .runForeach(println)

}
