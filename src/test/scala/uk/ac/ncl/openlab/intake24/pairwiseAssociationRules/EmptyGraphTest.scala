package uk.ac.ncl.openlab.intake24.pairwiseAssociationRules

import org.scalatest.FunSuite

/**
  * Created by Tim Osadchiy on 04/10/2017.
  */
trait EmptyGraphTest extends FunSuite {

  test("Empty graph behaviour test") {
    val p = PairwiseAssociationRules(None)
    p.addTransactions(Seq(Seq("a", "b", "c"), Seq("a", "b")))
    val params = p.getParams()
    assert(params.occurrences("a") == 2 &&
      params.coOccurrences("a")("c") == 1)
  }

}
