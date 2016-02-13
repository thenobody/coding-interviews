package net.thenobody.codinginterviews.linkedlists

/**
 * Created by antonvanco on 11/02/2016.
 */
object LinkedListsScala {

  def removeAllOccurrences(value: String, values: List[String]): List[String] = values match {
    case head +: tail if head == value => removeAllOccurrences(value, tail)
    case head +: tail => head +: removeAllOccurrences(value, tail)
    case Nil => Nil
  }

  def removeDuplicates(input: List[String]): List[String] = input match {
    case head +: tail =>
      head +: (removeAllOccurrences(head, tail) match {
        case newTail if newTail.nonEmpty => removeDuplicates(newTail)
        case _ => Nil
      })
    case Nil => Nil
  }

  def getKthElement(k: Int, input: List[String]): String = {
    def go(items: List[String]): Unit = items match {
      case head +: tail => go(tail)
      case _ =>
    }
    input match {
      case head +: _ if k == 0 => head
      case head +: tail =>
        getKthElement(k, tail)
    }
  }

  def deleteElement(element: String, list: List[String]): List[String] = list match {
    case head +: tail if head == element => tail
    case head +: tail => head +: deleteElement(element, tail)
  }

  def main(args: Array[String]): Unit = {
  }

}
