package cats.typeclasses

import java.io.FileOutputStream
import java.nio.ByteBuffer
import cats.typeclasses.Channels.ChannelImpl_3.{ByteEncoder, FileChannel, StringByteEncoder}
import scala.util.Using

object Channels extends App {

  object ChannelImpl_1 {

    trait Channel {
      def write(obj: Any): Unit
    }
  }

  object ChannelImpl_2 {

    trait Channel {
      def write(obj: ByteEncodable): Unit
    }

    trait ByteEncodable {
      def encode(): Array[Byte]
    }
  }

  object ChannelImpl_3 {

    trait Channel {
      def write[A](obj: A, enc: ByteEncoder[A]): Unit
    }

    trait ByteEncoder[A] {
      def encode(a: A): Array[Byte]
    }

    object FileChannel extends Channel {
      override def write[A](obj: A, enc: ByteEncoder[A]): Unit = {

        val bytes: Array[Byte] = enc.encode(obj)

        Using(new FileOutputStream("test")) { os =>
          os.write(bytes)
          os.flush()
        }
      }
    }

    object IntByteEncoder extends ByteEncoder[Int] {
      override def encode(n: Int): Array[Byte] = {
        val bb = ByteBuffer.allocate(4)
        bb.putInt(n)
        bb.array()
      }
    }

    FileChannel.write[Int](5, IntByteEncoder)

    object StringByteEncoder extends ByteEncoder[String] {
      override def encode(s: String): Array[Byte] = {
        s.getBytes()
      }
    }

    FileChannel.write[String]("5", StringByteEncoder)
  }

  object ChannelImpl_4 {

    trait Channel {
      def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
    }

    object ByteEncoder {
      implicit object StringByteEncoder extends ByteEncoder[String] {
        override def encode(s: String): Array[Byte] = {
          s.getBytes()
        }
      }
    }

    trait ByteEncoder[A] {
      def encode(a: A): Array[Byte]
    }

    object FileChannel extends Channel {
      override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {

        val bytes: Array[Byte] = enc.encode(obj)

        Using(new FileOutputStream("test")) { os =>
          os.write(bytes)
          os.flush()
        }
      }
    }

    FileChannel.write[String]("5")
  }
}
