package Theory

/**
  * Created by niv on 06/02/2018.
  */
object TypeClass extends App {

  trait Printable[A] {
    def format(a: A): String
  }

  object PrintableInstances {
    implicit val printableString = new Printable[String] {
      override def format(a: String): String = a
    }

    implicit val printableInt = new Printable[Int] {
      override def format(a: Int): String = a.toString
    }

    implicit val printableCat = new Printable[Cat] {
      override def format(a: Cat): String = s"name=${a.name},age=${a.age},color=${a.color}"
    }
  }

  object Printable {
    def format[A](a: A)(implicit printer: Printable[A]): Unit = {
      println(printer.format(a))
    }
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](a: A) {
      def format(implicit p: Printable[A]): String = {
        p.format(a)
      }

      def print(implicit p: Printable[A]): Unit = {
        println(p.format(a))
      }
    }
  }

  final case class Cat(name: String, age: Int, color: String)

  import PrintableInstances._
  import PrintableSyntax._
  val cat = Cat("kitty", 1, "white")
  Printable.format(cat)
  cat.print
}
