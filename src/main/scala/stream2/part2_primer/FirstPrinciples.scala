package stream2.part2_primer

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}

import scala.concurrent.Future

/**
  * Created by niv on 12/21/2020
  */
object FirstPrinciples extends App {

  implicit val actorSystem = ActorSystem("FirstPrinciples")
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
  source.to(flowWithSink)
  source.via(flow).to(sink).run()

  // nulls are NOT allowed
  val illegalSource = Source.single[String](null)
  illegalSource.to(Sink.foreach(println)).run()
  // use Options instead

  // various kinds of sources
  val finiteSource = Source.single(1)
  val anotherFiniteSource = Source(List(1,2,3))
  val emptySource = Source.empty[Int]
  val infiniteSource = Source(scala.Stream.from(1)) // do not confuse an Akka stream with a "collection" Stream

  import scala.concurrent.ExecutionContext.Implicits.global
  val  futureSource = Source.fromFuture(Future(42))

  // sinks
  val theMostBoringSink = Sink.ignore
  val foreachSink = Sink.foreach[String](println)
  val headSink = Sink.head[Int] // retrieves head tand then closes the stream
  val foldSink = Sink.fold[Int, Int](0)((a,b) => a + b)

  // flows - usually mapped to collection operators
  val mapFlow = Flow[Int].map(x => x * 2)
  val takeFlow = Flow[Int].take(5)
  // drop, filter
  // NOT have flatMap

  // source -> flow -> flow - ... -> sink
  val doubleFlowGraph = source.via(mapFlow).via(takeFlow).to(sink)

  // syntatic sugars
  val mapSource = Source(1 to 10).map(x => x * 2) // Source(1 to 10).via(Flow[Int].map(x => x * 2))
  mapSource.runForeach(println) // mapSource.to(Sink.foreach[Int](println).run()

  // OPERATORS = components

  // Exercise - create a stream that takes the names of persons, then you will keep the first 2 names with length > 5 character

  Source(List("yooo", "aaaaaaa", "ddddddddd", "qqq")).filter(_.length > 5).take(2).to(Sink.foreach[String](println)).run()
}
