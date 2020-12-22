package stream2.part2_primer

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}

import scala.util.{Failure, Success}

/**
  * Created by niv on 12/22/2020
  */
object MaterializingStreams extends App {

  implicit val actorSystem = ActorSystem("MaterializingStreams")
  implicit val materializer = ActorMaterializer()

  val simpleGraph = Source(1 to 10).to(Sink.foreach(println))
  val simpleMaterializedValue = simpleGraph.run() // no meaningful value

  val source = Source(1 to 10)
  val sink = Sink.reduce[Int]((a,b) => a + b)
  val sumFuture = source.runWith(sink)

  import scala.concurrent.ExecutionContext.Implicits.global
  sumFuture.onComplete {
    case Success(value) => println(s"the sum is$value")
    case Failure(exception) => println(s"error occurred $exception")
  }

  // choosing materialized values
  val simpleSource = Source(1 to 10)
  val simpleFlow = Flow[Int].map(x => x + 1)
  val simpleSink = Sink.foreach[Int](println)
  simpleSource.viaMat(simpleFlow)((sourceMat, flowMat) => flowMat)
  simpleSource.viaMat(simpleFlow)(Keep.right)
  simpleSource.viaMat(simpleFlow)(Keep.both)
  simpleSource.viaMat(simpleFlow)(Keep.none)
  val graph = simpleSource.viaMat(simpleFlow)(Keep.right).toMat(simpleSink)(Keep.right)

  // sugars
//  val r = Source(1 to 10).toMat(Sink.reduce(_ + _))(Keep.right).run()
//  val r2 = Source(1 to 10).runWith(Sink.reduce(_ + _))
//  val r3 = Source(1 to 10).runReduce(_ + _)

  // backwards
  val r4 = Sink.foreach[Int](println).runWith(simpleSource)

  // both ways
  Flow[Int].map(x => x * 2).runWith(simpleSource, simpleSink)

  // always use viaMat and toMat as in -simpleSource.viaMat(simpleFlow)(Keep.right).toMat(simpleSink)(Keep.right)

  // Ex 1. return the last element out of a source (use Sink.last)
  //    2. compute the total word count out of a stream of sentences (use map, fold, reduce)

  val one = Source(List(1,2,3)).toMat(Sink.last)(Keep.right).run()
  val two = Source(List("aaa bbb","bbb aaaaa","aaa qq")).map(_.split(" ").length).runReduce(_ + _)
  val twoWithFold = Source(List("aaa bbb","bbb aaaaa","aaa qq")).toMat(Sink.fold[Int, String](0)((sum, str) => sum + str.split(" ").length))(Keep.right).run()
  val twoWithFoldFlow = Source(List("aaa bbb","bbb aaaaa","aaa qq")).viaMat(Flow[String].fold[Int](0)((sum, str) => sum + str.split(" ").length))(Keep.right).toMat(Sink.head)(Keep.right).run()


  two.onComplete {
    case Success(value) => println(value)
    case Failure(e) => println(e)
  }
}
