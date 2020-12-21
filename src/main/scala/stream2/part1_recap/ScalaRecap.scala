package stream2.part1_recap

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * Created by niv on 12/21/2020
  */
object ScalaRecap extends App {

  implicit val actorSystem = ActorSystem("Playground")
  implicit val materializer = ActorMaterializer()

  val aCondition: Boolean = false
  def myFunction(x: Int) = {
    // code
    if(x > 4) 42 else 65
  }
  // instructions vs expressions
  // types + type inference

  // OO features of Scala
  class Animal
  trait Carnivore {
    def eat(a: Animal): Unit
  }

  object Carnivore

  // generics
  abstract class MyList[+A]

  // method notations
  1 + 2
  1.+(2)

  // FP
  val anIncrementer: Int => Int = (x: Int) => x + 1
  anIncrementer(1)

  List(1,2,3).map(anIncrementer)
  // HOF: flatMap, filter
  // for-comprehensions

  // Monads: Option, Try

  // Pattern matching!
  val unknown: Any = 2
  val order = unknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  try {
    // code that can throw exception
    throw new RuntimeException
  } catch {
    case e: Exception => println("i caught one")
  }

  // Scala advance

  // multithreading

  import scala.concurrent.ExecutionContext.Implicits.global
  val future = Future {
    42
  }

  future.onComplete {
    case Success(value) => println(s"i found the meaning of life: $value")
    case Failure(exception) => println(s"i found the exception while searching for the meaning of life: $exception")
  }// on SOME thread

  val partialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 65
    case _ => 999
  }

  type AkkaReceive = PartialFunction[Any, Unit]
  def receive: AkkaReceive = {
    case 1 => println("hello")
    case _ => println("confused...")
  }

  // implicits
  implicit val timeout = 3000
  def setTimeout(f: () => Unit)(implicit timeout: Int) = f()

  setTimeout(() => println(timeout))

  // conversions
  // implicit methods
  case class Person(name: String) {
    def greet: String = "Hi"
  }

  implicit def fromStringToPerson(name: String) = Person(name)

  "peter".greet

  // implicit classes
  implicit class Dog(name: String) {
    def bark = println("Bark!")
  }

  "Lassie".bark

  // implicit organizations
  // local scope
  implicit val numberOrdering: Ordering[Int] =

}
