package stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}

/**
  * Created by niv on 20/04/2019.
  */
object MaterializingStreams extends App {

  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()
  import actorSystem.dispatcher

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

  val f1 = Source(1 to 10).toMat(Sink.last)(Keep.right).run()
  val f2 = Source(1 to 10).runWith(Sink.last)

  val setenceSource = Source(List(
    "Akka is awesome",
    "I love streams",
    "Materilized values are kiliiiiiing who 2222"
  ))

  val wordCountSink = Sink.fold[Int, String](0)((currentWords, newSentence) => currentWords + newSentence.split( "").length)
  val g1 = setenceSource.toMat(wordCountSink)(Keep.right).run()
  val g2 = setenceSource.runWith(wordCountSink)
  val g3 = setenceSource.runFold(0)((currentWords, newSentence) => currentWords + newSentence.split(" ").length)

  val wordCountFlow = Flow[String].fold[Int](0)((currentWords, newSentences) => currentWords + newSentences.split(" ").length)
  val g4 = setenceSource.via(wordCountFlow).toMat(Sink.head)(Keep.right).run()
  val g5 = setenceSource.viaMat(wordCountFlow)(Keep.right).toMat(Sink.head)(Keep.right).run()
  val g6 = setenceSource.via(wordCountFlow).runWith(Sink.head)
  val g7 = wordCountFlow.runWith(setenceSource, Sink.head)._2

  g4.onComplete {
    result => println(result)
  }

}
