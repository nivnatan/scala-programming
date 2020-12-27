package stream2.part2_primer

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Sink, Source}
import scala.concurrent.duration._

/**
  * Created by niv on 12/27/2020
  */
object BackpressureBasics extends App {

  implicit val actorSystem = ActorSystem("BackpressureBasics")
  implicit val materializer = ActorMaterializer()

  val fastSource = Source(1 to 1000)
  val slowSink = Sink.foreach[Int] { x =>
    // simulating long processing
    Thread.sleep(1000)
    println(s"Sink $x")
  }

  // backpressure is not applied as all run on the same actor !
  fastSource.to(slowSink).run()

  // fusion applied - backpressure is on
  fastSource.async.to(slowSink).run()

  val simpleFlow = Flow[Int].map { x =>
    println(s"Incoming $x")
    x + 1
  }

  // buffer internal number of elements - default is 16
  fastSource.async.via(simpleFlow).async.to(slowSink).run()

  // reactions to backpressure:
  // 1. try to slow down if possible
  // 2. buffer elements until there's more demand
  // 3. tear down/kill the whole stream (failure)

  val bufferFlow = simpleFlow.buffer(10, overflowStrategy = OverflowStrategy.dropHead)

  // buffer flow, receives all the 1000 elements from the source and prints "Incoming 1" until "Incoming 1000".
  // passes the first 16 elements, and then it drop new elements while buffering the last 10 elements
  // the sink prints "Sink: 2" to "Sink: 17" as the first 16 elements were buffered at the sink

  // 1-16: nobody is backpressured beacuse the sink buffered the first 16 elements
  // 17-26: flow will buffer. now because the source is so fast and the sink is slow, the flow will start dropping elements
  // 26-1000: flow will always drop the oldest elements
  // 991-1000 => 992 - 1001 => sink

  fastSource.async.via(bufferFlow).async.to(slowSink).run()

  // throttling
  fastSource.throttle(2, 1.seconds).runWith(Sink.foreach(println))

}
