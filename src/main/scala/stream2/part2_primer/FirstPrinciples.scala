package stream2.part2_primer

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}

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
  val infiniteSource = Source(scala.Stream.from(1)) // do not confuse an Akka stream with a "collection" Strea,

}
