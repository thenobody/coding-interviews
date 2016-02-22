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

  def funnyString(): Unit = {
    val count = io.StdIn.readInt()

    def go(input: String, reverse: String, acc: Boolean): Boolean = input.toList match {
      case j0 +: j1 +: Nil => (Math.abs(j1 - j0) == Math.abs(reverse.tail.head - reverse.head)) && acc
      case j0 +: j1 +: tail =>
        val matches = (Math.abs(j1 - j0) == Math.abs(reverse.tail.head - reverse.head)) && acc
        go(input.tail, reverse.tail, matches)
    }

    (0 until count).map(_ => io.StdIn.readLine()).foreach { line =>
      println(
        go(line, line.reverse, true) match {
          case true => "Funny"
          case _ => "Not Funny"
        }
      )
    }
  }

  def alternatingCharacters(): Unit = {
    def go(previous: Int, input: Seq[Char], deletions: Int): Int = {
      if (previous == input.size - 1) deletions
      else if (input(previous) == input(previous + 1)) go(previous + 1, input, deletions + 1)
      else go(previous + 1, input, deletions)
    }

    val count = io.StdIn.readInt()
    (0 until count).map(_ => io.StdIn.readLine()).foreach { line =>
      println(go(0, line, 0))
    }
  }

  def gameOfThrones1(): Unit = {
    val input = io.StdIn.readLine()
    input.toSeq.groupBy(identity).count(_._2.size % 2 == 1) match {
      case c if c > 1 => println("NO")
      case _ => println("YES")
    }
  }



  def main(args: Array[String]): Unit = {
    gameOfThrones1()
  }

}
