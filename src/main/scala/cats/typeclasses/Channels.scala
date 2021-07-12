package cats.typeclasses


import scala.util.Using

object Channels extends App {

  trait Channel {
    def write(obj: Any): Unit
  }

  object FileChannel extends Channel {
    override def write(obj: Any): Unit = {
      val bytes: Array[Byte] = ???


    }
  }

}
