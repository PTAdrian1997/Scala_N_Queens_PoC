package agents.logic

import agents.logic.ChessboardStateValidator.{StateInvalidResponse, StateInvalidResponses}
import agents.{EmptyNogoods, LocalView, LoggerName, Nogood, Nogoods, QueenAgent}
import exception.Exceptions.NoBacktrackingRequiredException
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.matchers.must.Matchers
import org.slf4j.{Logger, LoggerFactory}

class HyperResolutionRuleTest extends AnyFeatureSpec with GivenWhenThen with Matchers {

  implicit val logger: Logger = LoggerFactory.getLogger(LoggerName)

  Feature("QueenAgent.applyHyperResolutionRule") {
    val numRows: Int = 4
    val initialNogoods: List[Nogood] = List.empty[Nogood]
    Scenario("A problem with four queens, no complex Nogoods have been generated" +
      "before and the 3rd row (or with id = 2) calls it; There are also no conflicting Nogoods" +
      "that are actually involved in the generation process")
    {
      Given("A problem with 4 queens")
      val currentRow: Int = 2
      val agentView: LocalView = Map(0 -> 0, 1 -> 2, 3 -> 0)
      val nogoods: Nogoods = EmptyNogoods
      And("A list of state invalid responses that triggered the computation")
      val stateInvalidResponses: StateInvalidResponses = Array(
        StateInvalidResponse(Nogood(Map(0 -> 0))),
        StateInvalidResponse(Nogood(Map(1 -> 2))),
        StateInvalidResponse(Nogood(Map(1 -> 2))),
        StateInvalidResponse(Nogood(Map(1 -> 2)))
      )
      When("The hyper-resolution rule is applied")
      val actualResult: Nogoods = new HyperResolutionRule(
        Range(0, numRows).toArray, currentRow, agentView
      ).applyHyperResolutionRule(stateInvalidResponses)
      Then("The expected results are returned")
      val expectedResults: List[Nogood] = List(
        Nogood(Map(0 -> 0, 1 -> 2))
      )
      actualResult must contain theSameElementsAs expectedResults
    }
    Scenario("A problem with four queens; there are no conflicting nogoods involved" +
      "in the generation process, but there are some that must be merged")
    {
      Given("A problem with four queens in an inconsistent state")
      val agentView: LocalView = Map(0 -> 0, 1 -> 3, 2 -> 1)
      val nogoods: Nogoods = EmptyNogoods
      val currentRow: Int = 3
      And("A list of nogoods that triggered the computation process")
      val stateInvalidResponses: StateInvalidResponses = Array(
        StateInvalidResponse(Nogood(Map(0 -> 0))),
        StateInvalidResponse(Nogood(Map(1 -> 3))),
        StateInvalidResponse(Nogood(Map(2 -> 1))),
        StateInvalidResponse(Nogood(Map(1 -> 3)))
      )
      When("The hyper-resolution rule is applied")
      val actualResult: Nogoods = new HyperResolutionRule(Range(0, numRows).toArray, currentRow, agentView)
        .applyHyperResolutionRule(stateInvalidResponses)
      Then("The expected results are returned")
      val expectedNogoods: Nogoods = Array(Nogood(Map(0 -> 0, 1 -> 3, 2 -> 1)))
      actualResult must contain theSameElementsAs expectedNogoods
    }
    Scenario("A problem with 3 queens; The problematic row is row no. 2 (the third row);" +
      "All the nogoods are given; The hyper-resolution rule should generate an empty nogood") {
      val agentView: LocalView = Map(0 -> 0, 1 -> 2)
      val currentRow: Int = 2
      And("The list of nogoods that triggered the computations")
      val stateInvalidResponses: StateInvalidResponses = Array(
        StateInvalidResponse(Nogood(Map(0 -> 0))),
        StateInvalidResponse(Nogood(Map(0 -> 1))),
        StateInvalidResponse(Nogood(Map(0 -> 2)))
      )
      When("The hyper-resolution rule is applied")
      val actualResult: Nogoods = new HyperResolutionRule(
        Range(0, numRows).toArray, currentRow, agentView
      ).applyHyperResolutionRule(stateInvalidResponses)
      Then("The empty Nogood is returned")
      actualResult must contain theSameElementsAs List(Nogood(Map.empty[Int, Int]))
    }
  }

}
