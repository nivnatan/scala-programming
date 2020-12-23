package http

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.CompactByteString
import akka.http.scaladsl.server.Directives._

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
        handleWebSocketMessages(webSocketFlow)
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

  // create a flow that the input is a stream and the output is a source
  val socialFlow: Flow[Message, Message, Any] = Flow.fromSinkAndSource(
    Sink.foreach[Message](println),

  )
}
