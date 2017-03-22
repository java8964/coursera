package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {   
    @tailrec
    def balWithAccu(idx: Int, count: Int): Boolean = {
      if (idx == chars.length)
        count == 0
      else {
        chars(idx) match {
          case '(' => balWithAccu(idx+1, count+1)
          case ')' => {
            if (count == 0) 
              false
            else 
              balWithAccu(idx+1, count-1)
          }
          case _ => balWithAccu(idx+1, count)
        }
      }
    }
    
    balWithAccu(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(idx: Int, until: Int, leftCnt: Int, rightCnt: Int): (Int, Int) = {
      if (idx >= until)
        (leftCnt, rightCnt)
      else chars(idx) match {
        case '(' => traverse(idx+1, until, leftCnt+1, rightCnt)
        case ')' => if (leftCnt > 0) traverse(idx+1, until, leftCnt-1, rightCnt) else traverse(idx+1, until, leftCnt, rightCnt+1)
        case _ => traverse(idx+1, until, leftCnt, rightCnt)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val size = until - from
      if (size > threshold) {
        val ((leftCnt1, rightCnt1), (leftCnt2, rightCnt2)) = parallel(reduce(from, from + size / 2), reduce(from + size / 2, until))
        if (leftCnt1 >= rightCnt2)
          (leftCnt1 - rightCnt2 + leftCnt2, rightCnt1)
        else
          (leftCnt2, rightCnt2 - leftCnt1 + rightCnt1)
      } else {
        traverse(from, until, 0, 0)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
