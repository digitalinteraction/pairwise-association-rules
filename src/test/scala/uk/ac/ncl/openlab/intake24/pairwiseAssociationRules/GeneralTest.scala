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

  private def combine(combs: Seq[Cmb]) =
    combs.map(i => i.coOccurrences.toDouble / i.selfOccurrences).sum * combs.map(_.selfOccurrences).sum

  private def combine(comb: Cmb) = comb.coOccurrences.toDouble

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
        "b" -> combine(Cmb(1, 2)),
        "c" -> combine(Cmb(1, 2)),
        "d" -> combine(Cmb(2, 2)),
        "e" -> combine(Cmb(1, 2))
      ),
      Seq("d") -> Map(
        "a" -> combine(Cmb(2, 3)),
        "b" -> combine(Cmb(2, 3)),
        "c" -> combine(Cmb(1, 3)),
        "e" -> combine(Cmb(2, 3))
      ),
      Seq("a", "d") -> Map(
        "b" -> combine(Seq(Cmb(1, 2), Cmb(2, 3))),
        "c" -> combine(Seq(Cmb(1, 2), Cmb(1, 3))),
        "e" -> combine(Seq(Cmb(1, 2), Cmb(2, 3)))
      ),
      Seq("a", "d", "f") -> Map(
        "b" -> combine(Seq(Cmb(1, 2), Cmb(2, 3))),
        "c" -> combine(Seq(Cmb(1, 2), Cmb(1, 3))),
        "e" -> combine(Seq(Cmb(1, 2), Cmb(2, 3)))
      )
    ).map(i => i._1 -> i._2)

    val associationRules = PairwiseAssociationRules(initialTransactions)

    assertTask(associationRules, firstTask)
  }

  test("Add rules test") {
    val associationRules = PairwiseAssociationRules(initialTransactions)

    val newTrans = Seq("c", "e", "f")

    associationRules.addTransaction(newTrans)

    val extendedTransactions = initialTransactions ++ Seq(newTrans)
    val secondTask = Seq(
      Seq("e", "f") -> Map(
        "a" -> combine(Cmb(1, 3)),
        "d" -> combine(Cmb(2, 3)),
        "b" -> combine(Cmb(1, 3)),
        "c" -> combine(Seq(Cmb(1, 3), Cmb(1, 1)))
      )
    )

    assertTask(associationRules, secondTask)
  }

  case class Cmb(coOccurrences: Int, selfOccurrences: Int)

}
