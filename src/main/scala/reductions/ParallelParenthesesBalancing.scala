package reductions

import scala.annotation._
import org.scalameter._
import common._

import scala.collection.immutable.List

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
    def dijkstra(chars: List[Char], stack: List[Char]): Boolean =
      if (chars.isEmpty) {
        stack.isEmpty
      }
      else {
        chars.head match {
          case '(' => dijkstra(chars.tail, stack :+ chars.head)
          case ')' => if (stack.isEmpty)
            false
          else
            dijkstra(chars.tail, stack.dropRight(1))
          case _ => dijkstra(chars.tail, stack)
        }
      }

    dijkstra(chars.toList, List[Char]())
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int):(Int, Int) = {
      if(idx < until){
        chars(idx) match {
          case '(' => traverse(idx+1, until, arg1+1, arg2)
          case ')' => {
            if(arg1 > 0)
              traverse(idx+1, until, arg1-1, arg2)
            else
              traverse(idx+1, until, arg1, arg2+1)
          }
          case _ => (arg1, arg2)
        }
      } else {
        (arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int):(Int, Int) = {
      if((until - from) <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = (from + until) / 2
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))
        (left._1+right._1, if(left._2 != 0) left._2 else right._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
