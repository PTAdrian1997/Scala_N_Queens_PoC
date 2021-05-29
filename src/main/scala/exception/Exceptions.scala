package exception

import agents.LocalView


object Exceptions {

  class NoBacktrackingRequiredException(agentView: LocalView)
    extends Exception(s"The agentView $agentView can still lead to viable solutions")

}
