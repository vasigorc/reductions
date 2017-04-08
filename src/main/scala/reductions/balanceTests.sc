import scala.annotation.tailrec
import scala.collection.immutable.List

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

balance(Array('(', '(', ')'))