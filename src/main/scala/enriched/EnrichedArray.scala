package enriched

import agents.{EmptyIndexes, Index, Indexes}

import scala.annotation.tailrec

object EnrichedArray {

  implicit class EnrichedArray[T](arr: Array[T]) {

    lazy val length: Int = arr.length

    def findIndexesWhere(func: T => Boolean) : Indexes = {
      @tailrec
      def innerLoop(i: Index, acc: Indexes): Indexes =
        if(i == length) acc else if(func(arr(i))) innerLoop(i + 1, acc :+ i) else innerLoop(i + 1, acc)
      innerLoop(0, EmptyIndexes)
    }

    /**
     * Returns the first index from the collection where the condition is met
     * @param func a function that takes a value of type T as argument and returns a Boolean value
     * @return Some(index), if the element arr(index) is the first element from the collection that
     *         satisfies the condition, and None otherwise
     */
    def findFirstIndexWhere(func: T => Boolean) : Option[Index] = {
      @tailrec
      def innerLoop(i: Index): Option[Index] = if(i == length) None else if(func(arr(i))) Some(i) else innerLoop(i + 1)
      innerLoop(0)
    }

  }

}
