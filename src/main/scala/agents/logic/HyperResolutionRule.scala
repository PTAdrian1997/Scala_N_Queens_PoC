package agents.logic

import agents.logic.ChessboardStateValidator.{AcceptanceResponses, StateInvalidResponses}
import agents.{ColumnDomain, ColumnValueType, EmptyLocalViews, Index, Indexes, LocalView, LocalViews, Nogood, Nogoods}
import akka.actor.typed.scaladsl.LoggerOps
import constraints.ConflictAvoidingArgument
import exception.Exceptions.NoBacktrackingRequiredException

import scala.annotation.tailrec
import enriched.EnrichedMap._
import enriched.EnrichedArray._
import constraints.Constraint._
import org.slf4j.Logger

import scala.collection.immutable.{AbstractMap, SeqMap, SortedMap}

class HyperResolutionRule(domain: ColumnDomain,
                          currentRow: Int,
                          localView: LocalView)(implicit logger: Logger) {

  /**
   *
   * @param remainingNogoods
   * @return
   */
  private def unifySimilarNogoods(remainingNogoods: List[LocalView]): List[LocalView] = ???

  /**
   * Find, for each possible column value assignment, either an already existent Nogood that
   * is compatible with the current agent view (including the current row) or a constraint
   * that is not satisfied and convert it into a Nogood. If no such entity can be found,
   * then an NoBacktrackingRequiredException is thrown.
   *
   * @return the list of nogoods that prevent this agent view from reaching a solution,
   *         or NoBacktrackingRequiredException, if a solution is still reachable
   */
  private def nogoodsToConsider(nogoods: Nogoods): LocalViews = {
    val imaginaryLocalView: LocalView = localView - currentRow
    val validNogoods: Nogoods = nogoods.filter(_.positions.keySet.contains(currentRow))
      .sortBy(-_.positions.keySet.size)
    domain
      .map { colId =>
        validNogoods.find(nogood => nogood.checkCompatibility(localView)) match {
          case Some(actualNogood) => actualNogood.positions.view.filterKeys(_ != currentRow).toMap
          case None =>
            logger.log.debug(s"localView: $localView")
            imaginaryLocalView.find { case (otherRowId, otherColId) =>
              !ConflictAvoidingConstraint(
                ConflictAvoidingArgument(currentRow, colId, otherRowId, otherColId)).checkConstraint
            } match {
              case Some((otherRow, otherCol)) =>
                Map(otherRow -> otherCol)
              case None =>
                throw new NoBacktrackingRequiredException(imaginaryLocalView)
            }
        }
      }
  }.toArray

  /**
   * Merge the elements of the provided list of agent views, using the following merging rules:
   *  - if an agent view element can also be found in another element, then remove it from the list
   * @param assignmentLocalViews the list of local views to be processed
   * @return the list of agent views where the elements have been merged
   */
  private def mergeLocalViews(assignmentLocalViews: LocalViews): LocalViews = {
    @tailrec
    def innerLoop(index: Index, acc: LocalViews): LocalViews = {
      if (index < assignmentLocalViews.length) {
        if (acc.exists(assignmentLocalViews(index).toSet subsetOf _.toSet)) {
          /* If the head is already contained in an element from acc, use the more general nogood: */
          innerLoop(index + 1, acc)
        }
        else if (acc.exists(_.toSet subsetOf assignmentLocalViews(index).toSet)) {
          /* Again, use the more general nogood: */
          val indexToBeReplaced = acc.indexWhere(_.toSet subsetOf assignmentLocalViews(index).toSet)
          val newAcc = acc.updated(indexToBeReplaced, assignmentLocalViews(index))
          innerLoop(index + 1, newAcc)
        } else {
          /* merge the nogoods wherever there are no conflicts: */
          val newAcc: LocalViews = if (acc.exists(_.isNotConflicting(assignmentLocalViews(index)))) {
            acc.map {
              case localView: LocalView if localView.isNotConflicting(assignmentLocalViews(index)) =>
                localView ++ assignmentLocalViews(index)
              case localView: LocalView => localView
            }
          } else {
            acc :+ assignmentLocalViews(index)
          }
          innerLoop(index + 1, newAcc)
        }
      }
      else acc
    }
    innerLoop(0, EmptyLocalViews)
  }

  /**
   * Apply the hyper-resolution rule on the domain of the current row and on the list of nogoods
   * that has been accumulated by this agent
   *
   * @param acceptanceResponses A list of StateInvalidResponse instances containing the Nogoods that
   *                            identified the current lesser agent view (i.e. the position of the other
   *                            agents except for the caller) as a state that cannot lead to a solution
   * @return a list of nogoods
   */
  def applyHyperResolutionRule(acceptanceResponses: StateInvalidResponses): Array[Nogood] = {
    /*
    * Theoretically, this method should only be called when the current row cannot choose any column assignment,
    * therefore, for each possible column assignment, either a constraint is violated or a Nogood is compatible with
    * the corresponding agent view generated. If that's not the case, then the method should not have been
    * called.
    * */

    val assignmentLocalViews: LocalViews = acceptanceResponses.map(_.nogood.positions - currentRow)
    logger.debug(s"assignmentLocalViews: ${assignmentLocalViews.mkString("Array(", ", ", ")")}")

    val mergedLocalViews: LocalViews = mergeLocalViews(assignmentLocalViews)
    /* If there are nogoods that only differ in the value assignment of one
    * particular row and there's one such nogood for each possible column assignment
    * (i.e. the entire range of values is covered), then merge those nogoods into one,
    * without that value (this guarantees that, if there's no solution, then an
    * empty nogood is generated):
    *  */
    mergedLocalViews.map(Nogood)
  }


}
