package chatapp

import java.util.UUID

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.pathEndOrSingleSlash
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Sink, Source}

/**
  * Created by niv on 25/12/2020.
  */
object ChatApp extends App {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val htmlRoute: Route =
    get {
      pathEndOrSingleSlash {
        getFromResource("chatapp/index.html")
      } ~ path("room" / Remaining) { f =>
        getFromResource(s"chatapp/room/$f")
      } ~ path("chat") {
        parameters("room", "name") { (roomName, nickName) =>
          handleWebSocketMessages(ChatManager2.chatFlow)
        }
      }
    }

  val chatRef = system.actorOf(Props[ChatManager])

  def chatFlow(room: String, connectionId: UUID) : Flow[String, String, Any] = {

    val sink = Flow[String]
      .map(msg => Protocol.ChatMessage(room, connectionId, msg))
      .to(Sink.actorRef(chatRef, Protocol.ChatCloseMessage(room, connectionId)))

    val source = Source.actorRef(16, OverflowStrategy.fail)
      .mapMaterializedValue {
        actor : ActorRef => {
          chatRef ! Protocol.ChatOpenMessage(room, connectionId, actor)
        }
      }

    Flow.fromSinkAndSource(sink, source)
  }

  Http().bindAndHandle(htmlRoute, "localhost", 8080)
}

object Protocol {
  case class ChatMessage(room: String, connectionId: UUID, msg: String)
  case class ChatOpenMessage(room: String, connectionId: UUID, actor: ActorRef)
  case class ChatCloseMessage(room: String, connectionId: UUID)
}

class ChatManager extends Actor {

  private var rooms = scala.collection.mutable.Map.empty[String, ChatRoom]

  override def receive: Receive = {
    case msg: ChatMessage => {
      rooms.get(msg.room) match {
        case Some(room) =>
      }
    }
  }
}

class ChatRoom extends Actor {
  override def receive: Receive = ???
}

//case class ChatRoom(clients: Map[UUID, ActorRef])