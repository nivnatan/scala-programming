package stream2.part1_recap

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorLogging, ActorSystem, OneForOneStrategy, PoisonPill, Props, Stash, SupervisorStrategy}
import org.scalatest.concurrent.PatienceConfiguration.Timeout

/**
  * Created by niv on 12/21/2020
  */
object AkkaRecap extends App {

  class SimpleActor extends Actor with Stash with ActorLogging {
    override def receive: Receive = {
      case "createChild" =>
        val childActor = context.actorOf(Props[SimpleActor], "myChild")
      case "stashThis" =>
        stash()
      case "change handler NOW" =>
        unstashAll()
        context.become(anotherHandler)

      case "change" => context.become(anotherHandler)
      case message => println(s"I received: $message")
    }

    def anotherHandler: Receive = {
      case message => println(s"In another receive handler: $message")
    }

    override def preStart(): Unit = {
      println("im starting")
    }

    override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
      case _: RuntimeException => Restart
      case _ => Stop
    }
  }

  // actor encapsulation
  val system = ActorSystem("AkkaRecap")
  // 1. you can onlyinstantiate an actor through the actor system
  val actor = system.actorOf(Props[SimpleActor], "simpleActor")
  // 2. sending messages
  actor ! "hello"

  // - messages are sent asynchronously
  // - many actors (in the millions) can share a few dozen threads
  // - each message is processed/handled ATOMICALLY
  // - no need for locks

  // changing actor behavior + stashing
  // actors can spawn other actors
   // guardians: /system, /user, / = root guardian

  // actors have defined lifecycle: they can be started, stopped, suspended, resumed, restarted

  // stopping actors - context.stop
  actor ! PoisonPill

  // logging
  // supervision

  // configure Akka infrastructure: dispatchers, routers, mailboxes

  // schedulers
  import scala.concurrent.duration._
  import system.dispatcher
  system.scheduler.scheduleOnce(2.seconds) {
    actor ! "delayed happy birthsday!"
  }

  // Akka patterns including FSM + ask pattern
  import akka.pattern.ask
  implicit val timeout = Timeout(3 seconds)

  val future = actor ? "question"

  // the pipe pattern
  import akka.pattern.pipe
  val anotherActor = system.actorOf(Props[SimpleActor], "anotherSimpleActor")
  future.mapTo[String].pipeTo(anotherActor)
}
