package chatapp

import akka.NotUsed
import akka.actor.Actor
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}

/**
  * Created by niv on 25/12/2020.
  */
object ChatManager {
  case class ChatRoomRequest(room: String, name: String)

  def chatFlow(implicit mat: Materializer): Flow[Message, Message, NotUsed] = Flow[Message].map {
    case tm: TextMessage    =>
      TextMessage(Source.single("Server says back:") ++ tm.textStream ++ Source.single("!")) // tm.textStream is the content of the originated of what the client sent
    case bm: BinaryMessage  =>
      bm.dataStream.runWith(Sink.ignore) // other wise we can leak resources, drain the data
      TextMessage(Source.single("Server received a binary message"))
  }
}

class ChatManager extends Actor {

  override def receive: Receive = {
    case _ =>
  }
}
