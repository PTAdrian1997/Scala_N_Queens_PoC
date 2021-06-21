package agents.mailbox

import agents.ColumnValueType
import agents.QueenAgent.{QueenMessageAnswerNogood, QueenMessageAskOk, QueenMessageNoSolution}
import akka.actor.{ActorRef, ActorSystem}
import akka.dispatch.{Envelope, MailboxType, MessageQueue, ProducesMessageQueue}
import com.typesafe.config.Config

import scala.collection.mutable

trait SquashingMessageQueueSemantics

object QueenSquashMailbox {

  class SquashingMessageQueue extends MessageQueue with SquashingMessageQueueSemantics {

    private final val queue: mutable.Queue[Envelope] = mutable.Queue.empty[Envelope]

    override def enqueue(receiver: ActorRef, handle: Envelope): Unit = {
      handle.message match {
        case QueenMessageAskOk(rowId, colId) =>
          /* remove all the other Ok? messages sent by rowId from the queue: */
          queue.dequeueFirst {
            env => env.message match {
              case QueenMessageAskOk(rowId, otherColId) => true
              case _ => false
            }
          }
          queue.enqueue(handle)
        case QueenMessageAnswerNogood =>
          queue.enqueue(handle)
        case QueenMessageNoSolution =>
          queue.enqueue(handle)
      }
      queue.enqueue(handle)
    }

    override def dequeue(): Envelope = queue.dequeue()

    override def numberOfMessages: ColumnValueType = queue.size

    override def hasMessages: Boolean = queue.nonEmpty

    override def cleanUp(owner: ActorRef, deadLetters: MessageQueue): Unit =
      while (hasMessages){
        deadLetters.enqueue(owner, dequeue())
      }
  }

}

/**
 *
 */
class QueenSquashMailbox extends MailboxType with ProducesMessageQueue[QueenSquashMailbox.SquashingMessageQueue] {

  import QueenSquashMailbox._

  def this(settings: ActorSystem.Settings, config: Config) = this()

  override def create(owner: Option[ActorRef], system: Option[ActorSystem]): MessageQueue = new SquashingMessageQueue()
}