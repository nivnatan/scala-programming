package chatapp

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.pathEndOrSingleSlash
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer

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
          handleWebSocketMessages(ChatManager.chatFlow)
        }
      }
    }

  Http().bindAndHandle(htmlRoute, "localhost", 8080)
}
