package net.thenobody.codinginterviews.hackerrank.dynamicprogramming

/**
 * Created by antonvanco on 15/02/2016.
 */
object DynamicProgramming {

  def modifiedFibonacci(): Unit = {
    def compute(a: BigInt, b: BigInt, n: BigInt): BigInt = {
      def go(n0: BigInt, n1: BigInt, c: BigInt): BigInt = {
        if (c == n) n1 * n1 + n0
        else go(n1, n1 * n1 + n0, c + 1)
      }
      go(a, b, 3)
    }

    val (a, b, n) = io.StdIn.readLine().split(' ') match { case Array(aa, bb, nn) => (aa.toInt, bb.toInt, nn.toInt) }
    println(compute(a, b, n))
  }

  def main(args: Array[String]): Unit = {
    modifiedFibonacci()
  }

}
