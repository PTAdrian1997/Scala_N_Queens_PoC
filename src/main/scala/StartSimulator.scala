import agents.SystemManager
import akka.actor.typed.ActorSystem

object StartSimulator extends App {

  val actorSystem = ActorSystem(SystemManager(6), "system-manager")
//  val actorSystem = ActorSystem(SystemManager(8), "system-manager")

}
