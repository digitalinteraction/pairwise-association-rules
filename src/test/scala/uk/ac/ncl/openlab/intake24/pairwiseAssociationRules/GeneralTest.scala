package uk.ac.ncl.openlab.intake24.pairwiseAssociationRules

import org.scalactic.Equality
import org.scalatest.FunSuite

/**
  * Created by Tim Osadchiy on 03/10/2017.
  */
trait GeneralTest extends FunSuite {

  implicit val doubleEquality = new Equality[Double] {
    override def areEqual(a: Double, b: Any): Boolean = Math.abs(a - b.asInstanceOf[Double]) < 1e-8
  }

  private val initialTransactions = TestConst.initialTransactions

  private def combine(probabilities: Seq[Double], supports: Seq[Int], inputSize: Int, transactionsSize: Int) = {
    (1 - probabilities.foldLeft(1d)((a, b) => a * (1 - b))) * supports.map(s => s / transactionsSize.toDouble).sum / inputSize.toDouble
  }

  private def combine(probability: Double, support: Double, transactionsSize: Int) = probability * support / transactionsSize.toDouble

  private def assertTask(rules: PairwiseAssociationRules, tasks: Seq[(Seq[String], Map[String, Double])]) = {
    assert(tasks.forall { task =>
      val rec = rules.recommend(task._1)
      val filteredRecs = rec.filter { r =>
        task._2.get(r._1).exists(_ === r._2)
      }
      filteredRecs.size == task._2.size
    })
  }

  test("General test") {
    val firstTask = Seq(
      Seq("a") -> Map(
        "b" -> combine(1 / 2d, 1, initialTransactions.size),
        "c" -> combine(1 / 2d, 1, initialTransactions.size),
        "d" -> combine(2 / 2d, 2, initialTransactions.size),
        "e" -> combine(1 / 2d, 1, initialTransactions.size)
      ),
      Seq("d") -> Map(
        "a" -> combine(2 / 3d, 2, initialTransactions.size),
        "b" -> combine(2 / 3d, 2, initialTransactions.size),
        "c" -> combine(1 / 3d, 1, initialTransactions.size),
        "e" -> combine(2 / 3d, 2, initialTransactions.size)
      ),
      Seq("a", "d") -> Map(
        "b" -> combine(Seq(1 / 2d, 2 / 3d), Seq(1, 2), 2, initialTransactions.size),
        "c" -> combine(Seq(1 / 2d, 1 / 3d), Seq(1, 1), 2, initialTransactions.size),
        "e" -> combine(Seq(1 / 2d, 2 / 3d), Seq(1, 2), 2, initialTransactions.size)
      ),
      Seq("a", "d", "f") -> Map(
        "b" -> combine(Seq(1 / 2d, 2 / 3d), Seq(1, 2), 3, initialTransactions.size),
        "c" -> combine(Seq(1 / 2d, 1 / 3d), Seq(1, 1), 3, initialTransactions.size),
        "e" -> combine(Seq(1 / 2d, 2 / 3d), Seq(1, 2), 3, initialTransactions.size)
      )
    ).map(i => i._1 -> i._2)

    val associationRulesParams = PairwiseAssociationRulesConstructorParams(
      initialTransactions.size,
      PairwiseAssociationRules.buildOccurrenceMap(initialTransactions),
      PairwiseAssociationRules.buildCoOccurrenceMap(initialTransactions)
    )
    val associationRules = PairwiseAssociationRules(Some(associationRulesParams))

    assertTask(associationRules, firstTask)
  }

  test("Add rules test") {
    val associationRulesParams = PairwiseAssociationRulesConstructorParams(
      initialTransactions.size,
      PairwiseAssociationRules.buildOccurrenceMap(initialTransactions),
      PairwiseAssociationRules.buildCoOccurrenceMap(initialTransactions)
    )

    val associationRules = PairwiseAssociationRules(Some(associationRulesParams))

    val newTrans = Seq("c", "e", "f")

    associationRules.addTransaction(newTrans)

    val extendedTransactions = initialTransactions ++ Seq(newTrans)
    val secondTask = Seq(
      Seq("e", "f") -> Map(
        "a" -> combine(Seq(1 / 3d, 0d), Seq(1), 2, extendedTransactions.size),
        "d" -> combine(Seq(2 / 3d, 0d), Seq(2), 2, extendedTransactions.size),
        "b" -> combine(Seq(1 / 3d, 0d), Seq(1), 2, extendedTransactions.size),
        "c" -> combine(Seq(1 / 3d, 1d), Seq(1, 1), 2, extendedTransactions.size)
      )
    )

    assertTask(associationRules, secondTask)
  }

}
