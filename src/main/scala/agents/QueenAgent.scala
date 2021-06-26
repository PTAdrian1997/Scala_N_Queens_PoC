package agents

import agents.logic.ChessboardStateValidator.{EmptyStateInvalidResponses, StateInvalidResponses}
import agents.logic.{ChessboardStateValidator, HyperResolutionRule}
import agents.states.{FindingAgentsState, FindingStateAgentInput}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, LoggerOps}

import scala.annotation.tailrec
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.immutable.Queue

case class Nogood(positions: Map[Int, Int]) {

  /**
   * Check if the current NoGood instance is compatible with the provided Agent View
   *
   * @param agentView a mapping from row indices to column indices
   * @return true if the Nogood is compatible with the Agent View, false otherwise
   */
  def checkCompatibility(agentView: Map[Int, Int]): Boolean =
    if (!(positions.keySet subsetOf agentView.keySet)) {
      false
    }
    else {
      positions.forall { case (key, value) => agentView(key) equals value }
    }

  def isEmpty: Boolean = positions.isEmpty

  def getLowestPriorityAgentId: Int = positions.keySet.max
}

object QueenAgent {


  sealed trait QueenMessageT

  case class QueenMessageAskOk(rowId: Int, colId: Int) extends QueenMessageT

  case class QueenMessageAnswerNogood(nogood: Nogood, senderId: Int) extends QueenMessageT

  case class QueenMessageAddLink(senderId: Int) extends QueenMessageT

  case class QueenMessageNoSolution() extends QueenMessageT

  sealed case class ListingResponse(listing: Receptionist.Listing) extends QueenMessageT

  case class FoundAllAgents(agentId: Int) extends QueenMessageT

  implicit val logger: Logger = LoggerFactory.getLogger(LoggerName)

  def getQueenId(rowId: Int): String = s"queen$rowId"

  val queenServiceKeyString: Int => String = queenId => s"queenServiceKey$queenId"
  val queenServiceKey: Int => ServiceKey[QueenMessageT] = queenId =>
    ServiceKey[QueenMessageT](queenServiceKeyString(queenId))

  val listingAdapter: ActorContext[QueenMessageT] => ActorRef[Receptionist.Listing] =
    context => context.messageAdapter[Receptionist.Listing](ListingResponse)

  sealed case class QueenState(
                                context: ActorContext[QueenMessageT],
                                currentRow: Int,
                                currentCol: Int,
                                agentView: LocalView,
                                communicatedNogoods: Array[Nogood],
                                neighbours: Set[Int]
                              ) {
    def changeCol(newCol: Int): QueenState =
      QueenState(context, currentRow, newCol, agentView + (currentRow -> newCol),
        communicatedNogoods, neighbours)

    def changeAgentValue(rowId: Int, colId: Int): QueenState =
      QueenState(context, currentRow, currentCol, agentView + (rowId -> colId),
        communicatedNogoods, neighbours)

    def addNogood(newNogood: Nogood): QueenState = QueenState(context,
      currentRow, currentCol, agentView, communicatedNogoods :+ newNogood, neighbours)

    def addNeighbor(neighbor: Int): QueenState = QueenState(context,
      currentRow, currentCol, agentView, communicatedNogoods, neighbours + neighbor)

  }

  /**
   * Find a new position for the calling queen that is consistent with the provided local view,
   * or the nogoods that are compatible with the provided agent view that make it impractical
   *
   * @param lesserAgentView An agent view that doesn't contain the position of the calling queen
   *                        (i.e. a lesser agent view)
   * @param numRows         The number of rows in the problem
   * @param nogoods         The list of accumulated nogoods
   * @param rowId           The row id of the calling queen
   * @return a Right containing the position that can satisfy all the constraints and doesn't cause
   *         the augmented agent view to be compatible with a nogood from the list, or the nogoods
   *         that are compatible with the provided agent view
   */
  def findAcceptableSolution(lesserAgentView: LocalView, numRows: Int, nogoods: Nogoods,
                             rowId: Int): Either[StateInvalidResponses, ColumnValueType] = {
    val domain: ColumnDomain = ChessboardStateValidator.domain(numRows)

    @tailrec
    def innerLoop(index: Index, invalidResponses: StateInvalidResponses): Either[StateInvalidResponses, ColumnValueType] =
      if (index == domain.length) Left(invalidResponses)
      else {
        logger.debug(s"current trial value: ${domain(index)}")
        ChessboardStateValidator.stateIsAcceptableForAgent(nogoods, rowId, domain(index), lesserAgentView) match {
          case ChessboardStateValidator.StateValidResponse =>
            Right(domain(index))
          case invalidResponse: ChessboardStateValidator.StateInvalidResponse =>
            innerLoop(index + 1, invalidResponses :+ invalidResponse)
        }
      }

    innerLoop(0, EmptyStateInvalidResponses)
  }

  /**
   * Perform a backtracking step: Search for a new column position assignment; If one is found, then change the
   * current value to the new value; otherwise, generate a new nogood using the hyper-resolution rule and send it
   * to the lowest priority agent from it.
   *
   * @param newQueenState the most recent queen state of the calling agent
   * @param currentRow    the id of the calling agent, which is associated with its row number
   * @param numRows       The number of rows from the problem
   * @param queenRegistry The actor references of all the agents and their Ids
   * @return a new Behavior
   */
  def backtrack(context: ActorContext[QueenMessageT], newQueenState: QueenState, currentRow: Int, numRows: Int,
                queenRegistry: YellowBook): Behavior[QueenMessageT] = {
    val agentViewToTest: LocalView = newQueenState.agentView.view.filterKeys(_ != currentRow).toMap
    findAcceptableSolution(agentViewToTest, numRows, newQueenState.communicatedNogoods, currentRow) match {
      case Left(stateInvalidResponses: StateInvalidResponses) =>
        context.log.debug("No acceptable solution could be found; Emit a nogood;")
        /* Create a new Nogood using the hyper-resolution rule: */
        val newNogood: Nogood = new HyperResolutionRule(Range(0, numRows).toArray,
          currentRow, newQueenState.agentView).applyHyperResolutionRule(stateInvalidResponses).head
        context.log.debug(s"Found nogood: $newNogood")
        if (newNogood.isEmpty) {
          /* Broadcast to all agents that there is no available
          * solution for this problem: */
          queenRegistry.foreach { case (agentId, actorRef) if agentId != currentRow =>
            actorRef ! QueenMessageNoSolution()
          }
          /* Terminate: */
          Behaviors.stopped
        }
        else {
          /* Communicate the nogood to the lowest priority queen from the nogood: */
          queenRegistry(newNogood.getLowestPriorityAgentId) ! QueenMessageAnswerNogood(newNogood, currentRow)
          processMessages(
            currentRow,
            numRows,
            newQueenState,
            queenRegistry
          )
        }
      case Right(newColumnPosition: ColumnValueType) =>
        val newQueenState2: QueenState = newQueenState.changeCol(newColumnPosition)
        /* Send the new value as an Ok? message to all the lower priority queens connected: */
        newQueenState.neighbours.foreach { neighbourId =>
          context.log.debug(s"Send Ok($currentRow, $newColumnPosition) to $neighbourId")
          queenRegistry(neighbourId) ! QueenMessageAskOk(currentRow, newColumnPosition)
        }
        processMessages(
          currentRow,
          numRows,
          newQueenState2,
          queenRegistry
        )
    }
  }


  /**
   * Process all the messages that have been sent to this agent while it was still idle.
   * @param queenState The state of the agent
   * @param currentRow The row associated with this agent
   * @param numRows The number of rows from the problem specification
   * @param queenYellowBook The agents yellow book, that specifies the memory / ip address / some reference point
   *                        for the agent associated with some row index
   * @param messageQueue The queue that keeps track of all the messages that have been sent to this agent
   * @return A new behaviour that will start the real-time processing of the messages
   */
  def processQueue(queenState: QueenState, currentRow: Int, numRows: Int,
                   queenYellowBook: YellowBook, messageQueue: Queue[QueenMessageT]): Behavior[QueenMessageT] = {
    messageQueue.foreach(message => queenYellowBook(currentRow) ! message)
    processMessages(
      currentRow, numRows, queenState, queenYellowBook)
  }

  /**
   * The behavior responsible for finding all the available queens
   *
   * @param context    The actor context that corresponds to this queen
   * @param currentRow The row index associated with this queen
   * @param numRows    The number of rows on the current chessboard
   * @return The conflict solving behavior if all the queens have been found, and this same behavior
   *         otherwise
   */
  def findingQueensBehavior(
                             currentRow: Int,
                             numRows: Int,
                             queenYellowBook: YellowBook,
                             finishedAgents: Int,
                             messageQueue: Queue[QueenMessageT]
                           ): Behavior[QueenMessageT] =
    Behaviors.setup {
      context =>
        context.setLoggerName(LoggerName)
        Behaviors.receiveMessage {
          case ListingResponse(listing) =>
            Range(0, numRows)
              .find(rowId => listing.key.id.equals(queenServiceKeyString(rowId))) match {
              case Some(rowToAdd) =>
                val serviceInstances: Set[ActorRef[QueenMessageT]] =
                  listing.allServiceInstances(queenServiceKey(rowToAdd))
                if (serviceInstances.isEmpty) {
                  context.log.debug(s"somehow, the agent ${rowToAdd} is and is not in the listing; Try again")
                  context.system.receptionist ! Receptionist.Find(queenServiceKey(rowToAdd), listingAdapter(context))
                  findingQueensBehavior(currentRow, numRows, queenYellowBook, finishedAgents, messageQueue)
                }
                else {
                  val newYellowBook: YellowBook = queenYellowBook + (rowToAdd -> serviceInstances.head)
                  context.log.debug(s"found agent ${rowToAdd}, new YB size: ${newYellowBook.size}")
                  if (newYellowBook.size == numRows) {
                    context.log.debug("I finished")
                    /**
                     * Inform the other agents that you have found all the agents, yourself included:
                     */
                    Range(0, numRows).foreach {
                      case localRow if localRow == currentRow =>
                      case localRow =>
                        newYellowBook(localRow) ! FoundAllAgents(currentRow)
                    }
                    if(finishedAgents + 1 == numRows) {
                      /**
                       * Send the Ok? message to the lower neighbour, if the current queen doesn't have the lowest
                       * priority:
                       */
                      Range(currentRow + 1, numRows).foreach {
                        newYellowBook(_) ! QueenMessageAskOk(rowId = currentRow, colId = 0)
                      }
                      processQueue(QueenState(
                        context,
                        currentRow = currentRow,
                        currentCol = 0,
                        agentView = Map(currentRow -> 0),
                        communicatedNogoods = EmptyNogoods,
                        neighbours = Range(currentRow + 1, numRows).toSet,
                      ), currentRow, numRows, newYellowBook, messageQueue)
                    }
                    else {
                      findingQueensBehavior(currentRow, numRows, newYellowBook, finishedAgents + 1, messageQueue)
                    }
                  }
                  else {
                    findingQueensBehavior(currentRow, numRows, newYellowBook, finishedAgents, messageQueue)
                  }
                }
              case None =>
                context.log.info("The subscribed agent could not be identified")
                findingQueensBehavior(currentRow, numRows, queenYellowBook, finishedAgents, messageQueue)
            }
          case FoundAllAgents(_) =>
            if(finishedAgents + 1 == numRows && queenYellowBook.size == numRows){
              processQueue(QueenState(
                context,
                currentRow = currentRow,
                currentCol = 0,
                agentView = Map(currentRow -> 0),
                communicatedNogoods = EmptyNogoods,
                neighbours = Range(currentRow + 1, numRows).toSet,
              ), currentRow, numRows, queenYellowBook, messageQueue)
            }
            else {
              findingQueensBehavior(currentRow, numRows, queenYellowBook, finishedAgents + 1, messageQueue)
            }
          case message =>
            context.log.debug(s"$currentRow has received $message ahead of time")
            findingQueensBehavior(currentRow, numRows, queenYellowBook, finishedAgents, messageQueue :+ message)
        }
    }

  def apply(rowId: Int, numberOfQueens: Int): Behavior[QueenMessageT] = Behaviors.setup { context =>
    context.log.info(s"queen ${rowId} started")
    context.system.receptionist ! Receptionist.Register(queenServiceKey(rowId), context.self)
    Range(0, numberOfQueens).foreach { otherRow =>
      context.system.receptionist ! Receptionist.Find(queenServiceKey(otherRow), listingAdapter(context))
    }
//    findingQueensBehavior(rowId, numberOfQueens, Map.empty[Int, ActorRef[QueenMessageT]], 0,
//      Queue.empty[QueenMessageT])
    new FindingAgentsState().findAgents(FindingStateAgentInput(rowId, numberOfQueens,
      Map.empty[Int, ActorRef[QueenMessageT]], 0, Queue.empty[QueenMessageT]))
  }

  /**
   * Perform the actual Asynchronous Backtracking Algorithm.
   *
   * @param currentRow    the row associated with the calling agent
   * @param numRows       the number of rows in the current environment
   * @param queenRegistry a Map that links a row number to the corresponding queen agent (the calling
   *                      agent is missing)
   * @param queenState    the current state of the calling agent
   * @return
   */
  def processMessages(currentRow: Int,
                      numRows: Int,
                      queenState: QueenState,
                      queenRegistry: YellowBook): Behavior[QueenMessageT] =
    Behaviors.receiveMessage {
      case QueenMessageAskOk(otherRowId, otherColId) =>
        queenState.context.log.debug(s"$currentRow has received QueenMessageAskOk($otherRowId, $otherColId); " +
          s"old queen agent nogoods: ${queenState.communicatedNogoods.mkString("Array(", ", ", ")")}")
        /* Check if the value of the receiving queen is consistent with its new agent view: */
        val newQueenState: QueenState = queenState.changeAgentValue(otherRowId, otherColId)
        queenState.context.log.debug(s"new queen agent: $newQueenState")
        ChessboardStateValidator.stateIsAcceptableForAgent(
          newQueenState.communicatedNogoods,
          currentRow,
          newQueenState.currentCol,
          newQueenState.agentView - currentRow
        ) match {
          case ChessboardStateValidator.StateValidResponse =>
            newQueenState.context.log.debug("The current position is fine")
            processMessages(currentRow, numRows, newQueenState, queenRegistry)
          case _: ChessboardStateValidator.StateInvalidResponse =>
            /* Search for another value from this domain that is consistent with the new agent view
            * (except for the row of the calling queen)
            * */
            newQueenState.context.log.debug("proceed with backtrack")
            backtrack(newQueenState.context, newQueenState, currentRow, numRows, queenRegistry)
        }
      case QueenMessageAnswerNogood(nogood, senderId) =>
        queenState.context.log.debug(s"$currentRow received another nogood message $nogood from $senderId")
        /* add the new nogood to the current collection: */
        val newQueenState: QueenState =
          nogood.positions
            .foldLeft(queenState.addNogood(nogood)) {
              case (acc, (queenId, colVal)) =>
                if (!acc.agentView.contains(queenId)) {
                  //                  queenRegistry(queenId) ! QueenMessageAddLink(currentRow)
                  acc
                    .changeAgentValue(queenId, colVal)
                } else {
                  acc
                }
            }
        if (!nogood.checkCompatibility(newQueenState.agentView)) {
          queenState.context.log.debug(s"$currentRow is not compatible with the received nogood;")
          queenRegistry(senderId) ! QueenMessageAskOk(rowId = currentRow,
            colId = newQueenState.currentCol)
          processMessages(
            currentRow,
            numRows,
            newQueenState,
            queenRegistry
          )
        }
        else {
          queenState.context.log.debug(s"$currentRow is compatible with the new nogood; Backtrack")
          /* Search for a new, acceptable column position assignment: */
          backtrack(queenState.context, newQueenState, currentRow, numRows, queenRegistry)
        }
      case QueenMessageNoSolution() =>
        queenState.context.log.info(s"Agent $currentRow has received a QueenMessageNoSolution message; terminate;")
        Behaviors.stopped
      case QueenMessageAddLink(senderId) =>
        queenState.context.log.info(s"Agent $currentRow has received a QueenMessageAddLink($senderId) message;")
        /* Add a link from currentRow to senderId; */
        //        val newQueenState: QueenState = queenState.addNeighbor(senderId)
        queenRegistry(senderId) ! QueenMessageAskOk(rowId = currentRow, colId = queenState.currentCol)
        processMessages(currentRow, numRows, queenState, queenRegistry)
    }


}

