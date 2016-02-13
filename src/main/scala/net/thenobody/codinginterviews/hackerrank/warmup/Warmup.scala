package net.thenobody.codinginterviews.hackerrank.warmup

/**
 * Created by antonvanco on 13/02/2016.
 */
object Warmup {

  def readArray: (Int, Seq[Int]) = io.Source.stdin.getLines().take(2).toList match {
    case List(sizeString, valuesString) =>
      sizeString.toInt -> valuesString.split(' ').take(sizeString.toInt).map(_.toInt).toSeq
  }

  type Matrix = Seq[Seq[Int]]
  def readMatrix: (Int, Matrix) = {
    val size = io.StdIn.readInt()
    size -> (1 to size).map(_ => io.StdIn.readLine().split(' ').take(size).map(_.toInt).toSeq)
  }

  def plusMinusFractions(): Unit = {
    val formatter = new java.text.DecimalFormat("0.000000")
    val fractions = io.Source.stdin.getLines().take(2).toList match {
      case List(sizeString, valuesString) =>
        valuesString.split(' ').take(sizeString.toInt).map(_.toInt).groupBy {
          case value if value < 0 => -1
          case value if value > 0 => 1
          case _ => 0
        }.mapValues(_.length / sizeString.toDouble)
    }
    Seq(1, -1, 0).foreach { i => println(formatter.format(fractions.getOrElse(i, 0.0))) }
  }

  def stairCase(): Unit = {
    val size = io.StdIn.readInt()
    (0 until size).foreach(i =>
      println((0 until size - i - 1).map(_ => " ").mkString + (0 to i).map(_ => "#").mkString)
    )
  }

  def twelveToTwentyFour(): Unit = {
    val input = io.StdIn.readLine()
    val regex = "([0-9]{2}):([0-9]{2}):([0-9]{2})(AM|PM)".r
    println(input match {
      case regex(hours, minutes, seconds, "AM") if hours == "12" => s"00:$minutes:$seconds"
      case regex(hours, minutes, seconds, "PM") if hours != "12" => s"${hours.toInt + 12}:$minutes:$seconds"
      case regex(hours, minutes, seconds, _) => s"$hours:$minutes:$seconds"
    })
  }

  def main(args: Array[String]): Unit = {

  }

}
