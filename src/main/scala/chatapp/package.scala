import akka.actor.ActorRef

/**
  * Created by niv on 25/12/2020.
  */
package object chatapp {
  case class ChatMessage(room: String, name: String, msg: String)
  case class ChatOpenMessage(room: String, name: String, actor: ActorRef)
  case class ChatCloseMessage(room: String, name: String)
}
