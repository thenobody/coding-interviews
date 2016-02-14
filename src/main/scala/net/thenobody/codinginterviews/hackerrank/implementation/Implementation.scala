package net.thenobody.codinginterviews.hackerrank.implementation

/**
 * Created by antonvanco on 13/02/2016.
 */
object Implementation {

  def angryProfessor(): Unit = {
    val count = io.StdIn.readInt()
    (0 until count).map { _ =>
      val (students, threshold) = io.StdIn.readLine().split(' ') match { case Array(s, t) => (s.toInt, t.toInt) }
      val arrivals = io.StdIn.readLine().split(' ').toSeq.map(_.toInt).take(students)
      if (arrivals.count(_ < 1) < threshold) "YES"
      else "NO"
    }.foreach(println)
  }

  def sherlockAndTheBeast(): Unit = {
    val lines = io.StdIn.readInt()
    (0 until lines).map(_ => io.StdIn.readInt()).map { count =>
      (0 to count).reverse.toIterator.filter { c =>
        c >= 0 && c % 3 == 0 && (count - c) % 5 == 0
      }.take(1).toList match {
        case fives +: _ if fives > 0 => "5" * fives + "3" * (count - fives)
        case fives +: _ if count % 5 == 0  => "3" * count
        case _ => "-1"
      }
    }.foreach(println)
  }

  def utopianTree(): Unit = {
    def go(n: Int): Int = n match {
      case 0 => 1
      case x if x % 2 != 0 => go(x - 1) * 2
      case x => go(x - 1) + 1
    }
    val lines = io.StdIn.readInt()
    (0 until lines).map(_ => io.StdIn.readInt()).map(go).foreach(println)
  }

  def findDigits(): Unit = {
    def go(n: Int): Int = n.toString.split("").flatMap(x => scala.util.Try(x.toInt).toOption).count { x => x > 0 && n % x == 0 }
    val lines = io.StdIn.readInt()
    (0 until lines).map(_ => io.StdIn.readInt()).map(go).foreach(println)
  }

  def sherlockAndSquares(): Unit = {
    def go(values: (Long, Long)): Long = values match { case (min, max) =>
      Math.sqrt(max).toLong - Math.ceil(Math.sqrt(min)).toLong + 1
    }
    val lines = io.StdIn.readInt()
    (0 until lines).map(_ => io.StdIn.readLine().split(' ') match { case Array(x, y) => (x.toLong, y.toLong) }).map(go).foreach(println)
  }

  def serviceLane(): Unit = {
    val (segments, inputs) = io.StdIn.readLine().split(' ') match {
      case Array(s, i) => (s.toInt, i.toInt)
    }
    val widths = io.StdIn.readLine().split(' ').map(_.toInt).take(segments).toSeq
    def go(input: (Int, Int)): Int = input match { case (i, j) => (i to j).map(widths).min }

    (0 until inputs).map(_ => io.StdIn.readLine().split(' ') match { case Array(i, j) => (i.toInt, j.toInt) }).map(go).foreach(println)
  }

  def cutTheSticks(): Unit = {
    def go(sticks: Seq[Int]): Unit = {
      println(sticks.size)
      val min = sticks.min
      val cut = sticks.map(_ - min).filter(_ > 0)
      if (cut.nonEmpty) go(cut)
    }

    val count = io.StdIn.readInt()
    go(io.StdIn.readLine().split(' ').take(count).map(_.toInt).toSeq)
  }

  def chocolateFeast(): Unit = {
    def go(input: (Int, Int, Int)): Int = input match {
      case (pocket, price, discount) =>
        var bars = pocket / price

        var turnover = bars / discount
        var leftover = bars % discount
        bars += turnover
        while ((turnover + leftover) / discount > 0) {
          val sum = turnover + leftover
          turnover = sum / discount
          leftover = sum % discount
          bars += turnover
        }
        bars
    }
    val count = io.StdIn.readInt()
    (0 until count).map(_ => io.StdIn.readLine().split(' ') match {
      case Array(pocket, price, discount) => (pocket.toInt, price.toInt, discount.toInt)
    }).map(go).foreach(println)
  }

  def caesarCipher(): Unit = {
    val length = io.StdIn.readInt()
    val input = io.StdIn.readLine().substring(0, length)
    val rotation = io.StdIn.readInt()

    def go(input: List[Char], buffer: String): String = input match {
      case in if in.isEmpty => buffer
      case head +: tail if head.isUpper => go(tail, buffer + ((head - 65 + rotation) % 26 + 65).toChar)
      case head +: tail if head.isLower => go(tail, buffer + ((head - 97 + rotation) % 26 + 97).toChar)
      case head +: tail => go(tail, buffer + head)
    }
    println(go(input.toList, ""))
  }

  def theGridSearch(): Unit = {
    implicit class SubstringString(self: String) {
      def isSubstring(search: String): Seq[Int] = {
        def go(sub: String, contain: String, index: Int, indexes: Seq[Int]): Seq[Int] = contain match {
          case c if sub.isEmpty => 0 until c.length
          case c if c.length < sub.length => indexes
          case c if c.startsWith(sub) => go(sub, c.tail, index + 1, indexes :+ index)
          case c => go(sub, c.tail, index + 1, indexes)
        }

        go(self, search, 0, Seq())
      }
    }

    def patternPresent(grid: Seq[String], pattern: Seq[String]): Boolean = grid match {
      case _ if pattern.isEmpty => true
      case g if g.length < pattern.length => false
      case gHead +: gTail if pattern.head.isSubstring(gHead).nonEmpty =>
        pattern.head.isSubstring(gHead).exists { index =>
          val subpattern = pattern.tail
          if (gTail.length >= subpattern.length) {
            subpattern.zip(gTail).forall { case (subp, subg) => subp.isSubstring(subg).contains(index) }
          } else false
        }
      case _ => false
    }

    def searchPatternInGrid(grid: Seq[String], pattern: Seq[String]): Boolean = grid match {
      case _ if pattern.isEmpty => true
      case g if g.length < pattern.length => false
      case g if patternPresent(g, pattern) => true
      case gHead +: gTail => searchPatternInGrid(gTail, pattern)
    }

    val count = io.StdIn.readInt()
    (0 until count).foreach { _ =>
      val grid = io.StdIn.readLine().split(' ') match { case Array(height, width) =>
        (0 until height.toInt).map(_ => io.StdIn.readLine().substring(0, width.toInt))
      }
      val pattern = io.StdIn.readLine().split(' ') match { case Array(height, width) =>
        (0 until height.toInt).map(_ => io.StdIn.readLine().substring(0, width.toInt))
      }

      searchPatternInGrid(grid, pattern) match {
        case true => println("YES")
        case _ => println("NO")
      }
    }
  }

  def cavityMap(): Unit = {
    val size = io.StdIn.readInt()
    val depthMap: Seq[Int] = (0 until size).flatMap(_ => io.StdIn.readLine().toSeq.map(_.toString.toInt))

    def isCavity(x: Int, y: Int): Boolean = {
      Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).forall { case (xx, yy) =>
        depthMap(xx * size + yy) < depthMap(x * size + y)
      }
    }

    val go: PartialFunction[(Int, Int), Any] = {
      case (x, y) if x == 0 => depthMap(x * size + y)
      case (x, y) if y == 0 => depthMap(x * size + y)
      case (x, y) if x == size - 1 => depthMap(x * size + y)
      case (x, y) if y == size - 1 => depthMap(x * size + y)
      case (x, y) if isCavity(x, y) => "X"
      case (x, y) => depthMap(x * size + y)
    }

    val result = for {
      x <- 0 until size
      y <- 0 until size
    } yield go(x, y)

    result.grouped(size).map(_.mkString).foreach(println)
  }

  def main(args: Array[String]): Unit = {
    cavityMap()
  }

}
