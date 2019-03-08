package Theory

import scala.annotation.tailrec

object Lazy extends App {

  for {
    a <- List(1,2,3) if a % 2 == 0
  } yield a + 1
  List(1,2,3).withFilter(_ % 2 == 0).map(_ + 1)

  /*
    Exercise implement a lazily evaluated, singly linked STREAM of elements.

    naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers (potentially infinite!)
    naturals.take(100) // lazily evaluated stream of the first 100 naturals (finite streams)
    naturals.foreach(println) // will crash - infinite!
    naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
   */
  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B] // prepend operator
    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] // concatenate two streams

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A] //takes the first n elements out of this stream
    def takeList(n: Int): List[A] = take(n).toList()

    @tailrec
    final def toList[B >: A](acc: List[B] = Nil): List[B] = {
      if(isEmpty) acc
      else tail.toList(head :: acc)
    }
  }

  object EmptyStream extends MyStream[Nothing] {
    override def isEmpty: Boolean = true
    override def head: Nothing = throw new NoSuchElementException
    override def tail: MyStream[Nothing] = throw new NoSuchElementException

    override def #::[B >: Nothing](element: B): MyStream[B] = new Cons(element, this)
    override def ++[B >: Nothing](anotherStream: MyStream[B]): MyStream[B] = anotherStream

    override def foreach(f: Nothing => Unit): Unit = ()
    override def map[B](f: Nothing => B): MyStream[B] = this
    override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
    override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

    override def take(n: Int): MyStream[Nothing] = this
    override def takeList(n: Int): List[Nothing] = Nil
  }

  class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
    override def isEmpty: Boolean = false
    override val head: A = hd
    override lazy val tail: MyStream[A] = tl

    override def #::[B >: A](element: B): MyStream[B] = new Cons(element, this)
    override def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream)

    override def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }
    override def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f)) // preserves lazy evaluation
    override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
    override def filter(predicate: A => Boolean): MyStream[A] = {
      if(predicate(head)) new Cons(head, tail.filter(predicate))
      else tail.filter(predicate) // preserve lazy evaluation
    }

    override def take(n: Int): MyStream[A] =
      if(n <= 0) EmptyStream
      else if(n == 1) new Cons(head, EmptyStream)
      else new Cons(head, tail.take(n-1))
  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = {
      new Cons(start, MyStream.from(generator(start))(generator))
    }
  }

  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)
}
