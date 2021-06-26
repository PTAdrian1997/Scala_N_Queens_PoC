package agents.states

import agents.QueenAgent.{FoundAllAgents, ListingResponse, QueenMessageAskOk, QueenMessageT, QueenState, listingAdapter, processQueue, queenServiceKey, queenServiceKeyString}
import agents.{EmptyNogoods, LoggerName, YellowBook}

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import scala.collection.immutable.Queue

case class FindingStateAgentInput(currentRow: Int,
                                  numRows: Int,
                                  queenYellowBook: YellowBook,
                                  finishedAgents: Int,
                                  messageQueue: Queue[QueenMessageT])

class FindingAgentsState {

  /**
   * Change the state of the actor. The next state is selected with the help of inputObj
   * @param inputObj The FindingStateAgentInput instance containing all the information
   *                 gathered during the searching process
   * @param context The actor context object needed for logging
   * @return
   */
  def moveOnToProcessMessages(inputObj: FindingStateAgentInput,
                              context: ActorContext[QueenMessageT]): Behavior[QueenMessageT] = {
    /**
     * Send the Ok? message to the lower neighbour, if the current queen doesn't have the lowest
     * priority:
     */
    Range(inputObj.currentRow + 1, inputObj.numRows).foreach {
      inputObj.queenYellowBook(_) ! QueenMessageAskOk(rowId = inputObj.currentRow, colId = 0)
    }
    processQueue(QueenState(
      context,
      currentRow = inputObj.currentRow,
      currentCol = 0,
      agentView = Map(inputObj.currentRow -> 0),
      communicatedNogoods = EmptyNogoods,
      neighbours = Range(inputObj.currentRow + 1, inputObj.numRows).toSet,
    ), inputObj.currentRow, inputObj.numRows, inputObj.queenYellowBook, inputObj.messageQueue)
  }

  /**
   * Find the agents in the current environment based on their Registration Keys.
   * @param inputObj The FindingStateAgentInput instance containing the information accumulated
   *                 during the search
   * @return Either a recursive call or the next state, selected based on the information
   *         gathered during the search
   */
  def findAgents(inputObj: FindingStateAgentInput): Behavior[QueenMessageT] =
    Behaviors.setup {
      context =>
        context.setLoggerName(LoggerName)
        Behaviors.receiveMessage {
          case ListingResponse(listing) =>
            Range(0, inputObj.numRows)
              .find(rowId => listing.key.id.equals(queenServiceKeyString(rowId))) match {
              case Some(rowToAdd) =>
                val serviceInstances: Set[ActorRef[QueenMessageT]] =
                  listing.allServiceInstances(queenServiceKey(rowToAdd))
                if (serviceInstances.isEmpty) {
                  context.log.debug(s"somehow, the agent ${rowToAdd} is and is not in the listing; Try again")
                  context.system.receptionist ! Receptionist.Find(queenServiceKey(rowToAdd), listingAdapter(context))
                  findAgents(inputObj)
                }
                else {
                  val newYellowBook: YellowBook = inputObj.queenYellowBook + (rowToAdd -> serviceInstances.head)
                  context.log.debug(s"found agent ${rowToAdd}, new YB size: ${newYellowBook.size}")
                  if (newYellowBook.size == inputObj.numRows) {
                    context.log.debug("I finished")
                    /**
                     * Inform the other agents that you have found all the agents, yourself included:
                     */
                    Range(0, inputObj.numRows).foreach {
                      case localRow if localRow == inputObj.currentRow =>
                      case localRow =>
                        newYellowBook(localRow) ! FoundAllAgents(inputObj.currentRow)
                    }
                    if(inputObj.finishedAgents + 1 == inputObj.numRows) {
                      /**
                       * Send the Ok? message to the lower neighbour, if the current queen doesn't have the lowest
                       * priority:
                       */
                      Range(inputObj.currentRow + 1, inputObj.numRows).foreach {
                        newYellowBook(_) ! QueenMessageAskOk(rowId = inputObj.currentRow, colId = 0)
                      }
                      moveOnToProcessMessages(inputObj.copy(queenYellowBook = newYellowBook), context)
                    }
                    else {
                      findAgents(FindingStateAgentInput(
                        inputObj.currentRow, inputObj.numRows, newYellowBook, inputObj.finishedAgents + 1,
                        inputObj.messageQueue))
                    }
                  }
                  else {
                    findAgents(FindingStateAgentInput(inputObj.currentRow, inputObj.numRows, newYellowBook,
                      inputObj.finishedAgents, inputObj.messageQueue))
                  }
                }
              case None =>
                context.log.info("The subscribed agent could not be identified")
                findAgents(inputObj)
            }
          case FoundAllAgents(_) =>
            if(inputObj.finishedAgents + 1 == inputObj.numRows && inputObj.queenYellowBook.size == inputObj.numRows){
              moveOnToProcessMessages(inputObj, context)
            }
            else {
              findAgents(FindingStateAgentInput(inputObj.currentRow, inputObj.numRows, inputObj.queenYellowBook,
                inputObj.finishedAgents + 1, inputObj.messageQueue))
            }
          case message =>
            context.log.debug(s"${inputObj.currentRow} has received $message ahead of time")
            findAgents(FindingStateAgentInput(
              inputObj.currentRow, inputObj.numRows, inputObj.queenYellowBook, inputObj.finishedAgents,
              inputObj.messageQueue :+ message))
        }
    }

}
