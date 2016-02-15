package net.thenobody.codinginterviews.hackerrank.strings

/**
 * Created by antonvanco on 15/02/2016.
 */
object Strings {

  def pangram(): Unit = {
    def go(input: Set[Char], required: Seq[Char]): Boolean = required match {
      case _ if required.isEmpty => true
      case head +: tail if input.contains(head) => go(input - head, tail)
      case _ => false
    }
    val input = io.StdIn.readLine().collect { case c if c != ' ' => c.toLower }.toSet
    println(go(input, 'a' to 'z') match {
      case true => "pangram"
      case _ => "not pangram"
    })
  }

  def main(args: Array[String]): Unit = {
    pangram()
  }

}
