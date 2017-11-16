/**
  * Created by niv on 16/11/2017.
  */
object Exercises extends App {

  /**
    * @param str
    * @return true if string has all unique characters, otherwise false
    * examples:
      isUnique("ascvfghj") = true
      isUnique("") = true
      isUnique("asdgdea") = false
    */
  def isUnique(str: String): Boolean = {
    val set = scala.collection.mutable.Set.empty[Char]
    if(str == "") true
    else {
      str.forall { case c =>
        set.contains(c) match {
          case true  => false
          case false => {
            set += c
            true
          }
        }
      }
    }
  }

}
