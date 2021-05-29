package agents

import agents.QueenAgent.QueenMessageT
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, LoggerOps}


object SystemManager {

  trait AdminMessageT
  case class QueenTerminated(rowId: Int) extends AdminMessageT

  def updateStatus(numberOfAgentsAlive: Int): Behavior[AdminMessageT] =
    Behaviors.receive{
      case (context, queenTerminated: QueenTerminated) =>
        context.log.info(s"queen ${queenTerminated.rowId} was terminated;")
        if(numberOfAgentsAlive - 1 == 0){
          Behaviors.stopped
        } else {
          updateStatus(numberOfAgentsAlive - 1)
        }
    }

  def apply(numberOfQueens: Int): Behavior[AdminMessageT] =
    Behaviors.setup { context =>
      context.log.info("Create the queen agents")
      Range(0, numberOfQueens).foreach{rowId =>
        val queenActor: ActorRef[QueenMessageT] = context.spawn(
          QueenAgent(rowId, numberOfQueens),
          QueenAgent.getQueenId(rowId)
        )
        context.watchWith(queenActor, QueenTerminated(rowId))
      }
      updateStatus(numberOfQueens)
    }

}
