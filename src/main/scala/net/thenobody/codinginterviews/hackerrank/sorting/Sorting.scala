package net.thenobody.codinginterviews.hackerrank.sorting

/**
 * Created by antonvanco on 15/02/2016.
 */
object Sorting {

  def tutorialChallenge(): Unit = {
    def go(start: Int, end: Int, needle: Int, haystack: Seq[Int]): Int = needle match {
      case _ if start == end - 1 && haystack(start) == needle => start
      case _ if start == end - 1 => -1
      case _ =>
        val half = end - (end - start) / 2
        if (needle == haystack(half)) half
        else if (needle < haystack(half)) go(start, half, needle, haystack)
        else go(half + 1, end, needle, haystack)
    }
    val needle = io.StdIn.readInt()
    val size = io.StdIn.readInt()
    val haystack = io.StdIn.readLine().split(' ').map(_.toInt).toSeq
    println(go(0, size, needle, haystack))
  }

  def main(args: Array[String]): Unit = {
    tutorialChallenge()
  }
}
