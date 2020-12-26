package chatapp

import java.util.UUID

import akka.NotUsed
import akka.actor.{Actor, ActorRef}
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



object ChatManger {

  def chatActorFlow(room: String, connectionId: UUID) : Flow[String, String, Any] = {

    val sink = Flow[String]
      .map(msg => Protocol.ChatMessage(room, connectionId, msg))
      .to(Sink.actorRef(chatRef, Protocol.CloseConnection(connectionId)))

    val source = Source.actorRef(16, OverflowStrategy.fail)
      .mapMaterializedValue {
        actor : ActorRef => {
          chatRef ! Protocol.OpenConnection(actor, connectionId)
        }
      }

    Flow.fromSinkAndSource(sink, source)
  }

}

class ChatManager extends Actor {

  override def receive: Receive = {
    case _ =>
  }

  private def rooms(rooms: Map[String, ActorRef]): Receive = {
    case _ =>
  }
}
