package uk.ac.ncl.openlab.intake24.pairwiseAssociationRules

import org.scalactic.Equality
import org.scalatest.FunSuite

/**
  * Created by Tim Osadchiy on 29/07/2018.
  */
trait ReduceTest extends FunSuite {

  test("Reduce test") {
    val params = PairwiseAssociationRulesConstructorParams(
      100,
      Map(
        "a" -> 10,
        "b" -> 20,
        "c" -> 70
      ),
      Map(
        "a" -> Map("b" -> 3, "c" -> 7),
        "b" -> Map("a" -> 3, "c" -> 10),
        "c" -> Map("a" -> 7, "b" -> 10)
      )
    )
    val graph = PairwiseAssociationRules(Some(params))
    val reduced = graph.reduce(3)
    val reducedParams = reduced.getParams()

    assert(
      reducedParams.coOccurrences("a").get("b").isEmpty &&
        reducedParams.coOccurrences("a")("c") == 7 &&
        reducedParams.coOccurrences("b").get("a").isEmpty &&
        reducedParams.coOccurrences("b")("c") == 10 &&
        reducedParams.coOccurrences("c")("a") == 7 &&
        reducedParams.coOccurrences("c")("b") == 10
    )
  }

}
