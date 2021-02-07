package stream2.part3_graphs

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

/**
  * Created by niv on 1/31/2021
  */
object MoreOpenGraphs extends App {

  implicit val actorSystem = ActorSystem("MoreOpenGraphs")
  implicit val materializer = ActorMaterializer()

  // example: Max3 Operator
  // - 3 inputs of type int
  // - the maximum of the 3

}
