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

  def reverseShuffleMerge(): Unit = {
    def removeFirst(needle: Char, haystack: Seq[Char]): Seq[Char] = haystack match {
      case _ if haystack.isEmpty => haystack
      case head +: tail if head == needle => tail
      case head +: tail => head +: removeFirst(needle, tail)
    }

    def isInterspersed(needle: Seq[Char], haystack: Seq[Char]): Boolean = needle match {
      case Nil => true
      case head +: tail =>
        val index = haystack.indexOf(head)
        if (index < 0) false
        else isInterspersed(tail, haystack.drop(index + 1))
    }

    def sub(prev: Seq[Char], remaining: Seq[Char], haystack: Seq[Char]): Option[Seq[Char]] = {
      if (!isInterspersed(prev.reverse, haystack)) None
      else if (remaining.isEmpty) Some(prev)
      else remaining.distinct.foldLeft[Option[Seq[Char]]](None) { case (acc, c) =>
        if (acc.isDefined) acc
        else sub(prev :+ c, removeFirst(c, remaining), haystack)
      }
    }

    val input = io.StdIn.readLine().toSeq
    val charCounts = input.groupBy(identity).mapValues(_.length / 2)

    val half = charCounts.keys.toSeq.sorted.flatMap { c => (0 until charCounts(c)).map(_ => c) }
    println(half)
    val result = sub(Seq(), half, input).get

    println(result.mkString)
  }

  def main(args: Array[String]): Unit = {
    reverseShuffleMerge()
  }

}
