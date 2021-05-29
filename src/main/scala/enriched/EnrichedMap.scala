package enriched

import agents.LocalView

object EnrichedMap {

  implicit class EnrichedMap(int2IntMap: LocalView) {

    /**
     *
     * @param other
     * @return
     */
    def isNotConflicting(other: LocalView): Boolean = {
      val commonKeys: Set[Int] = int2IntMap.keySet.intersect(other.keySet)
      commonKeys.forall(key => int2IntMap(key) == other(key))
    }

    /**
     *
     * @param other
     * @param exceptForKey
     * @return
     */
    def isSimilarWith(other: LocalView, exceptForKey: Int): Boolean =
      (int2IntMap - exceptForKey).equals(other - exceptForKey)

  }

}
