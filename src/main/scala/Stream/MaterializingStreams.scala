package Stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}

/**
  * Created by niv on 20/04/2019.
  */
object MaterializingStreams extends App {

  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()

  // choosing materialized values
  val simpleSource = Source(1 to 10)
  val simpleFlow = Flow[Int].map(x => x + 1)
  val simpleSink = Sink.foreach[Int](println)

  simpleSource.viaMat(simpleFlow)((sourceMat, flowMat) => flowMat)
  simpleSource.viaMat(simpleFlow)(Keep.right) // shortcut
  val graph = simpleSource.viaMat(simpleFlow)(Keep.right).toMat(simpleSink)(Keep.right)

  // sugars
  //Source(1 to 10).runWith(Sink.reduce(_ + _)) // source.to(Sink.reduce)(Keep.right)
  Source(1 to 10).runReduce(_ + _) // shortcut

  //backwards
  Sink.foreach[Int](println).runWith(Source.single(42)) // source(..).to(sink...).run()

  // both ways
  Flow[Int].map(x => 2 * x).runWith(simpleSource, simpleSink)

  /**
    * - return the last element out of source (use Sink.last)
    * - compute the total word count out of a stream of sentences
    * map, fold, reduce
    */
}
