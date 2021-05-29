package agents

import agents.QueenAgent.{QueenMessageT}
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.ActorContext
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import akka.actor.testkit.typed.internal._
import akka.actor.testkit.typed.scaladsl.ActorTestKit
import exception.Exceptions.NoBacktrackingRequiredException

class QueenAgentTest extends AnyFeatureSpec with GivenWhenThen with Matchers {

  info("As a user")
  info("I want to be able to use the Queen Agents")
  info("To their full functionality")

  val testkit = ActorTestKit

}
