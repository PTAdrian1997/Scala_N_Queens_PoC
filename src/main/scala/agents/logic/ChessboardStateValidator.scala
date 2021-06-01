package agents.logic

import agents.logic.ChessboardStateValidator.{AcceptanceResponseT, StateInvalidResponse, StateValidResponse}
import agents.{ColumnDomain, ColumnValueType, LocalView, Nogood, Nogoods}
import constraints.{ConflictAvoidingArgument, Constraint}
import org.slf4j.Logger

object ChessboardStateValidator {
  sealed trait AcceptanceResponseT
  type AcceptanceResponses = Array[AcceptanceResponseT]

  case object StateValidResponse extends AcceptanceResponseT

  sealed case class StateInvalidResponse(nogood: Nogood) extends AcceptanceResponseT
  type StateInvalidResponses = Array[StateInvalidResponse]
  val EmptyStateInvalidResponses: StateInvalidResponses = Array.empty[StateInvalidResponse]

  lazy val domain: Int => ColumnDomain = numRows => Range(0, numRows).toArray

  /**
   * Try to find the nogood that is compatible with the provided augmented agent view
   * @param nogoods the list of nogoods discovered up until now
   * @param augmentedAgentView the agent view of the caller agent (augmented means that it also contains it's key)
   * @param callerId the key of the caller agent
   * @return a StateInvalidResponse instance with the nogood that is compatible with the augmented agent view,
   *         or StateValidResponse, if no such nogood could be found
   */
  private def nogoodsAreIncompatible(nogoods: Nogoods, augmentedAgentView: LocalView, callerId: Int) (implicit logger: Logger): AcceptanceResponseT = {
    require(augmentedAgentView.contains(callerId), "The augmented agent view must contain the caller agent's id")
    logger.debug(s"augmentedAgentView: $augmentedAgentView")
    nogoods.sortBy(-_.positions.size).find(currentNogood => currentNogood.positions.keySet.contains(callerId) &&
      currentNogood.checkCompatibility(augmentedAgentView)) match {
      case Some(compatibleNogood) => StateInvalidResponse(compatibleNogood)
      case None => StateValidResponse
    }
  }

  /**
   * Check if all constraints are satisfied by this lesser agent view, and if not, return the constraint
   * that was violated in the form of a nogood
   * @param callerId the key of the calling agent
   * @param callerValue the value associated to the calling agent
   * @param lesserAgentView the agent view that doesn't contain the calling agent's key
   * @return StateInvalidResponse with the broken constraint, or StateValidResponse, if no unsatisfied
   *         constraint could be found
   */
  private def allConstraintsAreSatisfied(callerId: Int, callerValue: ColumnValueType,
                                 lesserAgentView: LocalView)(implicit logger: Logger): AcceptanceResponseT = {
    require(!lesserAgentView.contains(callerId), "The lesser agent view must not contain the caller agent's Id")
    logger.debug(s"lesserAgentView: $lesserAgentView")
    lesserAgentView.find {
      case (otherKey, _) if otherKey == callerId => false
      case (otherKey, otherValue) if otherKey != callerId =>
        !Constraint
          .ConflictAvoidingConstraint(ConflictAvoidingArgument(callerId, callerValue, otherKey, otherValue))
          .checkConstraint
    } match {
      case Some((otherKey, otherValue)) => StateInvalidResponse(Nogood(Map(callerId -> callerValue,
        otherKey -> otherValue)))
      case None => StateValidResponse
    }
  }

  /**
   * Check if the current position of the calling agent is acceptable and can lead to a solution
   * @param nogoods The list of nogoods accumulated up until now
   * @param callerKey The key of the caller agent
   * @param callerValue The value of the caller agent
   * @param lesserAgentView the agent view of the calling agent without its key
   * @return a StateInvalidResponse instance if the state or agent view is not consistent with
   *         the calling agent's position, or StateValidResponse otherwise
   */
  def stateIsAcceptableForAgent(nogoods: Nogoods, callerKey: Int, callerValue: ColumnValueType,
                                lesserAgentView: LocalView)(implicit logger: Logger): AcceptanceResponseT = {
    nogoodsAreIncompatible(nogoods, lesserAgentView + (callerKey -> callerValue), callerKey) match {
      case stateInvalidResponse: StateInvalidResponse => stateInvalidResponse
      case StateValidResponse =>
        allConstraintsAreSatisfied(callerKey, callerValue, lesserAgentView)
    }
  }

}
