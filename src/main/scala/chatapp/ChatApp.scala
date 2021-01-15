package chatapp

import java.util.UUID

import akka.NotUsed
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.server.Directives.pathEndOrSingleSlash
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Sink, Source}
import chatapp.ChatApp.chatRef
import chatapp.Protocol.{ChatFlowFactoryRequest, ChatFlowFactoryResponse}

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
  case class ChatFlowFactoryRequest(room: String, name: String)
  case class ChatFlowFactoryResponse(flow: Either[String, Flow[Message, Message, NotUsed]])
  case class ChatMessage(room: String, connectionId: UUID, msg: String)
  case class ChatOpenMessage(room: String, connectionId: UUID, actor: ActorRef)
  case class ChatCloseMessage(room: String, connectionId: UUID)
}

class ChatManager extends Actor {

  private var rooms = scala.collection.mutable.Map.empty[String, ChatRoom]

  override def receive: Receive = {

    case ChatFlowFactoryRequest(room, name) => {
      rooms.get(room) match {
        case Some(r) if r.users.contains(name) => sender() ! ChatFlowFactoryResponse(Left("name already in use"))
        case Some(r) => {




        }
      }
    }


    case msg: ChatMessage => {
      rooms.get(msg.room) match {
        case Some(room) =>
      }
    }
  }

  private def chatFlow(room: String, connectionId: UUID) : Flow[String, String, Any] = {

    val sink = Flow[String]
      .map(msg => Protocol.ChatMessage(room, connectionId, msg))
      .to(Sink.actorRef(self, Protocol.ChatCloseMessage(room, connectionId)))

    val source = Source.actorRef(16, OverflowStrategy.fail)
      .mapMaterializedValue {
        actor : ActorRef => {
          chatRef ! Protocol.ChatOpenMessage(room, connectionId, actor)
        }
      }

    Flow.fromSinkAndSource(sink, source)
  }
}

case class ChatRoom(users: Map[String, ActorRef])

//case class ChatRoom(clients: Map[UUID, ActorRef])