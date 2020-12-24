package http

import akka.NotUsed
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.util.CompactByteString
import akka.http.scaladsl.server.Directives._
import http.WebSocketDemo.ActorWebSocketHandler.{ActorWebSocketConnect, ActorWebSocketHandler}

import scala.concurrent.duration._

/**
  * Created by niv on 12/23/2020
  */
object WebSocketDemo extends App {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  // Message: TextMessage vs BinaryMessage
  val textMessage = TextMessage(Source.single("hello via a text message"))
  val binaryMessage = BinaryMessage(Source.single(CompactByteString("hello via a binary message")))

  val html =
    """
      |<html>
      |    <head>
      |        <script>
      |
      |            var exampleSocket = new WebSocket("ws://localhost:8080/greeter");
      |            console.log("starting web socket...");
      |
      |            exampleSocket.onmessage = function(event) {
      |                var newChild = document.createElement("div");
      |                newChild.innerText = event.data;
      |                document.getElementById("1").appendChild(newChild);
      |            };
      |
      |            exampleSocket.onopen = function(event) {
      |                exampleSocket.send("socket seems to be opened");
      |            };
      |
      |            exampleSocket.send("socket says: hello, server!")
      |
      |        </script>
      |    </head>
      |
      |    <body>
      |        starting websocket...
      |        <div id="1">
      |        </div>
      |    </body>
      |
      |</html>
    """.stripMargin

    val webSocketFlow: Flow[Message, Message, Any] =  Flow[Message].map {
      case tm: TextMessage    =>
        TextMessage(Source.single("Server says back:") ++ tm.textStream ++ Source.single("!")) // tm.textStream is the content of the originated of what the client sent
      case bm: BinaryMessage  =>
        bm.dataStream.runWith(Sink.ignore) // other wise we can leak resources, drain the data
        TextMessage(Source.single("Server received a binary message"))
    }

    val webSocketRoute =
      (pathEndOrSingleSlash & get) {
        complete(
          HttpEntity(
            ContentTypes.`text/html(UTF-8)`,
            html
        ))
      } ~
      path("greeter") {
        //handleWebSocketMessages(webSocketFlow)
        handleWebSocketMessages(socialFlow)
      }

  Http().bindAndHandle(webSocketRoute, "localhost", 8080)

  case class SocialPost(owner: String, content: String)

  val socialFeed = Source(
    List(
      SocialPost("Martin", "Scala 3 has been announced!"),
      SocialPost("Daniel", "A new Rock the JVM course is open!"),
      SocialPost("Martin", "I killed Java!")
    )
  )

  val socialMessages = socialFeed
    .throttle(1, 2.seconds)
    .map(socialPost => TextMessage(s"${socialPost.owner} said: ${socialPost.content}"))

  // construct a flow of messages to the web client
  // create a flow that the input is a stream and the output is a source
  val socialFlow: Flow[Message, Message, Any] = Flow.fromSinkAndSource(

    Sink.foreach[Message](println), // for every message from the client, println
    socialMessages // outgoing messages from the server to the client
  )

//  // interact with actors
//
//  // source - will be used as a gateway messages from server to client
//  val (actorRef, publisher) = Source.actorRef[String](16, OverflowStrategy.fail)
//    .map(msg => TextMessage.Strict(msg))
//    .toMat(Sink.asPublisher(false))(Keep.both).run()
//
//  // sink - will be used as a gateway from client to server
//  val sink = Sink.foreach[Message] {
//    case tm: TextMessage =>
//      TextMessage(Source.single("Server says back:") ++ tm.textStream ++ Source.single("!")) // tm.textStream is the content of the originated of what the client sent)
//  }

  def getActorFlow = {

    val handler = system.actorOf(Props[ActorWebSocketHandler])

    val source: Source[Message, NotUsed] =
      Source.actorRef[String](16, OverflowStrategy.fail)
        .mapMaterializedValue { userActor =>
          handler ! ActorWebSocketConnect(userActor)
          NotUsed
        }
        .map(msg => TextMessage.Strict(msg))

    val sink: Sink[Message, Any] = Sink.foreach[Message](println)

    Flow.fromSinkAndSource(sink, source)
  }

  object ActorWebSocketHandler {
    case class ActorWebSocketConnect(actorRef: ActorRef)

    class ActorWebSocketHandler extends Actor {
      private var connectorRef: ActorRef = _

      override def receive: Receive = {
        case ActorWebSocketConnect(ref) => connectorRef = ref
      }
    }
  }
}
