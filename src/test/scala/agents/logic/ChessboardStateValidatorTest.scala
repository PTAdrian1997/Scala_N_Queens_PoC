package agents.logic

import agents.logic.ChessboardStateValidator.{AcceptanceResponseT, StateInvalidResponse, StateValidResponse}
import agents.{EmptyLocalView, EmptyNogoods, LocalView, Nogood, Nogoods}
import org.scalatest.{GivenWhenThen, PrivateMethodTester}
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.matchers.must.Matchers

class ChessboardStateValidatorTest extends AnyFeatureSpec with GivenWhenThen with Matchers
 with PrivateMethodTester {

  Feature("ChessboardStateValidator.nogoodsAreIncompatible"){
    val numRows: Int = 4
    Given(s"$numRows rows")
    val chessboardStateValidator: ChessboardStateValidator = new ChessboardStateValidator(numRows)
    Scenario("The list of nogoods is empty and the augmented view is empty"){
      Given("An empty list of nogoods")
      val nogoods: Nogoods = EmptyNogoods
      And("An empty augmented agent view")
      val agentView: LocalView = EmptyLocalView
      val callerId: Int = 0
      And(s"$callerId as The callerId")
      When("nogoodsAreIncompatible is called")
      val thrown = intercept[IllegalArgumentException]{
        val nogoodsAreIncompatible = PrivateMethod[AcceptanceResponseT](Symbol("nogoodsAreIncompatible"))
        val value = chessboardStateValidator invokePrivate nogoodsAreIncompatible(nogoods, agentView, callerId)
      }
      Then("It must throw an IllegalArgumentException exception")
      thrown must have message "requirement failed: The augmented agent view must contain the caller agent's id"
    }
    Scenario("The list of nogoods is empty and the augmented view doesn't contain the callerId"){
      Given("An empty list of nogoods")
      val nogoods: Nogoods = EmptyNogoods
      And("An agent view that doesn't contain the callerId")
      val agentView: LocalView = Map(1 -> 3, 2 -> 1, 3 -> 3)
      val callerId: Int = 0
      And(s"$callerId as callerId")
      When("nogoodsAreIncompatible is called")
      val thrown = intercept[IllegalArgumentException]{
        val nogoodsAreIncompatible = PrivateMethod[AcceptanceResponseT](Symbol("nogoodsAreIncompatible"))
        chessboardStateValidator invokePrivate nogoodsAreIncompatible(nogoods, agentView, callerId)
      }
      Then("It must throw an IllegalArgumentException exception")
      thrown must have message "requirement failed: The augmented agent view must contain the caller agent's id"
    }
    Scenario("The list of nogoods is empty and the augmented view contains the callerId"){
      Given("An empty list of nogoods")
      val nogoods: Nogoods = EmptyNogoods
      And("An agent view that contains the caller id")
      val agentView: LocalView = Map(0 -> 0, 1 -> 3, 2 -> 1, 3 -> 3)
      val callerId: Int = 0
      And(s"$callerId as the callerId")
      When("The nogoodsAreIncompatible method is called")
      val nogoodsAreIncompatible = PrivateMethod[AcceptanceResponseT](Symbol("nogoodsAreIncompatible"))
      val actualResult: AcceptanceResponseT = chessboardStateValidator
        .invokePrivate(nogoodsAreIncompatible(nogoods, agentView, callerId))
      Then("The result should be the StateValidResponse object")
      actualResult mustBe StateValidResponse
    }
    Scenario("There's only one nogood that is compatible with the agent view"){
      Given("An augmented agent view")
      val agentView: LocalView = Map(0 -> 0, 1 -> 3, 2 -> 1, 3 -> 3)
      val callerId: Int = 0
      And(s"$callerId as callerId")
      And("A list of nogoods that only contains one compatible nogood")
      val nogoods: Nogoods = Array(
        Nogood(Map(0 -> 0, 3 -> 0)),
        Nogood(Map(1 -> 3, 3 -> 3)),
        Nogood(Map(0 -> 0, 1 -> 3, 2 -> 1))
      )
      When("The nogoodsAreIncompatible method is called")
      val nogoodsAreIncompatible = PrivateMethod[AcceptanceResponseT](Symbol("nogoodsAreIncompatible"))
      val actualResult: AcceptanceResponseT = chessboardStateValidator
        .invokePrivate(nogoodsAreIncompatible(nogoods, agentView, callerId))
      Then("The result must be a StateInvalidResponse containing the compatible nogood")
      actualResult mustBe StateInvalidResponse(Nogood(Map(0 -> 0, 1 -> 3, 2 -> 1)))
    }
    Scenario("There are multiple compatible nogoods of different sizes in the list"){
      Given("An augmented agent view")
      val agentView: LocalView = Map(0 -> 0, 1 -> 3, 2 -> 1, 3 -> 3)
      And("A list that contains compatible nogoods of different sizes")
      val nogoods: Nogoods = Array(
        Nogood(Map(1 -> 3, 3 -> 3)),
        Nogood(Map(0 -> 0, 1 -> 3, 2 -> 1))
      )
      val callerId: Int = 0
      And(s"A caller id equal to $callerId")
      When("The nogoodsAreIncompatible method is called")
      val nogoodsAreIncompatible = PrivateMethod[AcceptanceResponseT](Symbol("nogoodsAreIncompatible"))
      val actualResult: AcceptanceResponseT = chessboardStateValidator.invokePrivate(nogoodsAreIncompatible(nogoods, agentView, callerId))
      Then("The result should be a StateInvalidResponse containing the compatible nogood with the " +
        "greatest number of keys")
      actualResult mustBe StateInvalidResponse(Nogood(Map(0 -> 0, 1 -> 3, 2 -> 1)))
    }
  }

  Feature("ChessboardStateValidator.allConstraintsAreSatisfied"){
    val numRows: Int = 5
    Given(s"$numRows rows")
    val chessboardStateValidator: ChessboardStateValidator = new ChessboardStateValidator(numRows)
    Scenario("The agent view provided contains the caller id"){
      Given("An agent view that contains the caller id")
    }
    Scenario("In the agent view there aren't any violated constraints"){

    }
    Scenario("In the agent view there are violated constraints, but they don't involve the caller"){

    }
    Scenario("In the agent view there are multiple violated constraints that involve the caller"){

    }
  }

}
