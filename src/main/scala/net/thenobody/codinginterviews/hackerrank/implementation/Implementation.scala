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

  def manasaAndStones(): Unit = {
    def go(n: Int, a: Int, b: Int): Seq[Int] = n match {
      case _ if n == 1 => Seq(0)
      case _ => go(n - 1, a, b).flatMap { last => Seq(last + a, last + b) }.distinct.sorted
    }

    val count = io.StdIn.readInt()
    (0 until count).foreach { _ =>
      val n = io.StdIn.readInt()
      val a = io.StdIn.readInt()
      val b = io.StdIn.readInt()
      println(go(n, a, b).mkString(" "))
    }
  }

  def libraryFine(): Unit = {
    val (aDay, aMonth, aYear) = io.StdIn.readLine().split(' ') match { case Array(d, m, y) => (d.toInt, m.toInt, y.toInt) }
    val (eDay, eMonth, eYear) = io.StdIn.readLine().split(' ') match { case Array(d, m, y) => (d.toInt, m.toInt, y.toInt) }

    val aTimestamp = aYear * 10000 + aMonth * 100 + aDay
    val eTimestamp = eYear * 10000 + eMonth * 100 + eDay

    val dDiff = aDay - eDay
    val mDiff = aMonth - eMonth
    val yDiff = aYear - eYear

    println(
      if (aTimestamp <= eTimestamp) 0
      else if (yDiff > 0) 10000
      else if (mDiff > 0) mDiff * 500
      else if (dDiff > 0) dDiff * 15
    )
  }

  def acmIcpcTeam(): Unit = {
    val (people, topics) = io.StdIn.readLine().split(' ') match { case Array(p, t) => (p.toInt, t.toInt) }
    val skills = (0 until people).map(_ => io.StdIn.readLine().substring(0, topics))
    val ranks = for {
      i <- 0 until people
      j <- i + 1 until people
    } yield (0 until topics).count { t => skills(i)(t) == '1' || skills(j)(t) == '1' }

    val max = ranks.max
    println(max)
    println(ranks.count(_ == max))
  }

  def extraLongFactorials(): Unit = {
    def go(n: Int, acc: BigInt): BigInt = {
      if (n == 0) acc
      else go(n - 1, acc * n)
    }
    val n = io.StdIn.readInt()
    println(go(n, 1))
  }

  def taumAndBDay(): Unit = {
    val count = io.StdIn.readInt()
    (0 until count).foreach { _ =>
      val (bCount, wCount) = io.StdIn.readLine().split(' ') match { case Array(b, w) => (b.toLong, w.toLong) }
      val (bCost, wCost, xCost) = io.StdIn.readLine().split(' ') match { case Array(b, w, x) => (b.toLong, w.toLong, x.toLong) }

      println(
        if (bCost + xCost < wCost) (bCount + wCount) * bCost + wCount * xCost
        else if (wCost + xCost < bCost) (bCount + wCount) * wCost + bCount * xCost
        else bCount * bCost + wCount * wCost
      )
    }
  }

  def theTimeInWords(): Unit = {
    val numerals = Map(
      1 -> "one",
      2 -> "two",
      3 -> "three",
      4 -> "four",
      5 -> "five",
      6 -> "six",
      7 -> "seven",
      8 -> "eight",
      9 -> "nine",
      10 -> "ten",
      11 -> "eleven",
      12 -> "twelve",
      13 -> "thirteen",
      14 -> "fourteen",
      15 -> "quarter",
      16 -> "sixteen",
      17 -> "seventeen",
      18 -> "eighteen",
      19 -> "nineteen",
      20 -> "twenty",
      30 -> "half"
    )
    val hours = io.StdIn.readInt()
    val minutes = io.StdIn.readInt()

    println(
      (hours, minutes) match {
        case _ if minutes == 0 => numerals(hours) + " o' clock"
        case _ if minutes > 30 =>
          val remaining = 60 - minutes
          val mins = if (remaining > 20) numerals(remaining / 10 * 10) + " " + numerals(remaining % 10) + " minutes"
          else if (remaining == 15) numerals(remaining)
          else if (remaining == 1) numerals(remaining) + " minute"
          else numerals(remaining) + " minutes"
          mins + " to " + numerals(hours + 1)
        case _ =>
          val mins = if (minutes == 30) numerals(minutes)
          else if (minutes > 20) numerals(minutes / 10 * 10) + " " + numerals(minutes % 10) + " minutes"
          else if (minutes == 15) numerals(minutes)
          else if (minutes == 1) numerals(minutes) + " minute"
          else numerals(minutes) + " minutes"
          mins + " past " + numerals(hours)
      }
    )
  }

  def modifiedKaprekarNumbers(): Unit = {
    def isKaprekar(n: Long): Boolean = {
      val digits = n.toString.length
      val square = n * n
      val right = square % Math.pow(10, digits).toLong
      val left = square / Math.pow(10, digits).toLong
      left + right == n
    }

    val lowest = io.StdIn.readLong()
    val highest = io.StdIn.readLong()

    println(
      (lowest to highest).collect { case x if isKaprekar(x) => x } match {
        case numbers if numbers.isEmpty => "INVALID RANGE"
        case numbers  => numbers.mkString(" ")
      }
    )
  }

  def encryption(): Unit = {
    val input = io.StdIn.readLine()
    val length = input.length.toDouble
    val square = Math.sqrt(length)
    val columns = Math.min(Math.ceil(square), Math.ceil(length / square)).toInt

    val split = input.grouped(columns).toList
    val rows = split.size

    println(
      (for {
        c <- 0 until columns
        r <- 0 until rows
        if c < split(r).length
      } yield {
        val space = if (c > 0 && r == 0) " " else ""
        space + split(r)(c)
      }).mkString
    )
  }

  def matrixRotation(): Unit = {
    def getDirection(x: Int, y: Int, width: Int, height: Int, offset: Int): (Int, Int) = {
      val (xHalf, yHalf) = (width / 2.0, height / 2.0)
      val xMin = offset
      val xMax = width - offset - 1
      val yMin = offset
      val yMax = height - offset - 1

      if (y < yHalf) {                           // upper half
        if (x == xMin) (0, 1)                     // upper left corner OR bellow upper left corner
        else if (y == yMin && x == xMax) (-1, 0)  // upper right corner
        else if (x == xMax) (0, -1)               // below upper right corner
        else (-1, 0)                              // to the right of upper left corner
      } else {                                   // lower half
        if (x == xMax) (0, -1)                    // lower right corner OR above lower right corner
        else if (y == yMax && x == xMin) (1, 0)   // lower left corner
        else if (x == xMin) (0, 1)                // above left corner
        else (1, 0)                               // to the left of lower right corner
      }
    }

    def getOffset(x: Int, y: Int, width: Int, height: Int): Int = {
      val flip = (n: Int, half: Int) => if (n < half) n else 2 * half - n - 1
      Math.min(flip(x, width / 2), flip(y, height / 2))
    }

    type Matrix = Map[(Int, Int), String]

    def rotateMatrix(matrix: Matrix, width: Int, height: Int, steps: Int): Matrix = steps match {
      case 0 => matrix
      case _ =>
        rotateMatrix(matrix.map {
          case ((x, y), value) =>
            val offset = getOffset(x, y, width, height)
            val (xDiff, yDiff) = getDirection(x, y, width, height, offset)
            (x + xDiff, y + yDiff) -> value
        }, width, height, steps - 1)
    }

    def printMatrix(matrix: Map[(Int, Int), String], width: Int, height: Int): Unit = println(
      (0 until height).map { y =>
        (0 until width).map { x => matrix((x, y)) }.mkString(" ")
      }.mkString("\n")
    )

    def buildMatrix(input: Seq[Seq[String]], width: Int, height: Int): Matrix = (for {
      y <- 0 until height
      x <- 0 until width
    } yield (x, y) -> input(y)(x)).toMap

    val input = Seq(
      Seq("A", "B", "C", "D", "E", "F", "G", "H"),
      Seq("X", "1", "2", "3", "4", "5", "6", "I"),
      Seq("W", "7", "Q", "W", "E", "R", "7", "J"),
      Seq("V", "6", "I", "U", "Y", "T", "8", "K"),
      Seq("U", "5", "4", "3", "2", "1", "9", "L"),
      Seq("T", "S", "R", "Q", "P", "O", "N", "M")
    )

    val width = 8
    val height = 6
    val original = buildMatrix(input, width, height)

    printMatrix(original, width, height)
    println()
    printMatrix(original.map { case ((x, y), _) => (x, y) -> getOffset(x, y, width, height).toString }, width, height)
    println()
    val rotated = rotateMatrix(original, width, height, 3)
    printMatrix(rotated, width, height)
  }

  def main(args: Array[String]): Unit = {
    matrixRotation()
  }

}
