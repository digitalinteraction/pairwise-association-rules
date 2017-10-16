package uk.ac.ncl.openlab.intake24.pairwiseAssociationRules

import org.scalatest.FunSuite

/**
  * Created by Tim Osadchiy on 03/10/2017.
  */
trait NoNullDivisionTest extends FunSuite {

  private def assertTask(params: PairwiseAssociationRulesConstructorParams) = {
    val associationRules = PairwiseAssociationRules(Some(params))
    assert(associationRules.recommend(Seq("a", "b", "c")).isEmpty)
  }

  test("No null division by occurrences") {
    val associationRulesParams = PairwiseAssociationRulesConstructorParams(
      TestConst.initialTransactions.size,
      PairwiseAssociationRules.buildOccurrenceMap(TestConst.initialTransactions).map(n => n._1 -> 0),
      PairwiseAssociationRules.buildCoOccurrenceMap(TestConst.initialTransactions)
    )
    assertTask(associationRulesParams)

  }

}
