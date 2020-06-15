package uk.ac.ncl.openlab.intake24.pairwiseAssociationRules

import org.scalatest.FunSuite

import scala.collection.mutable
import scala.util.Random


class ImplTest extends FunSuite {

  val ids: IndexedSeq[String] = Range(1, 3000).map("%08d".format(_))

  val random = new Random()

  def randomTransaction(length: Int): Seq[String] = Seq.fill(length)(ids(random.nextInt(ids.size)))

  def randomTransactions(length: Int, count: Int): Seq[Seq[String]] = Seq.fill(count)(randomTransaction(length))

  def buildOccurrenceMap(transactions: Seq[Seq[String]]): Map[String, Int] =
    transactions.map(_.distinct).foldLeft(Map[String, Int]().withDefaultValue(0)) { (mp, itemSet) =>
      itemSet.foldLeft(mp) { (mp, item) =>
        mp + (item -> (mp(item) + 1))
      }
    }


  def buildOccurrenceMap2(transactions: Seq[Seq[String]]): mutable.Map[String, Int] = {
    val result = collection.mutable.Map[String, Int]()

    transactions.foreach { tx =>
      tx.distinct.foreach { id =>
        result.put(id, result.getOrElse(id, 0) + 1)
      }
    }

    result
  }

  def buildOccurrenceMap4(transactions: Seq[Seq[String]]): Map[String, Int] =
    transactions.foldLeft(Map[String, Int]()) { (acc, tx) =>
      tx.distinct.foldLeft(acc) { (acc, id) =>
        acc + (id -> (acc.getOrElse(id, 0) + 1))
      }
    }


  def buildOccurrenceMap3(transactions: Seq[Seq[String]]): Map[String, Int] = {

    def impl(transactions: Seq[Seq[String]], acc: Map[String, Int]): Map[String, Int] = transactions match {
      case tx :: txs =>
        val updated = tx.distinct.foldLeft(acc) { (acc, tx) => acc + (tx -> (acc.getOrElse(tx, 0) + 1)) }
        impl(txs, updated)
      case Nil => acc
    }

    impl(transactions, Map())
  }

  def buildCoOccurrenceMap(transactions: Seq[Seq[String]]): Map[String, Int] =
    transactions.map(_.distinct).foldLeft(
      Map[String, Int]().withDefaultValue(0)) {
      (occMap, transaction) =>
        PairwiseAssociationRulesUtils.pairs(transaction).foldLeft(occMap) {
          (occMap, pair) => {
            val key = (pair._1 + pair._2)
            occMap + (key -> (occMap(key) + 1))
          }
        }
    }

  def buildCoOccurrenceMap2(transactions: Seq[Seq[String]]): mutable.Map[String, Int] = {
    val result = mutable.Map[String, Int]()

    transactions.foreach { tx =>
      val distinct = tx.distinct

      for (id1 <- distinct;
           id2 <- distinct) {
        if (id1 != id2) {
          val key = id1 + id2;
          result.put(key, result.getOrElse(key, 0) + 1)
        }
      }
    }

    result
  }

  def buildCoOccurrenceMap3(transactions: Seq[Seq[String]]): Map[String, Int] =
    transactions.foldLeft(Map[String, Int]()) { (acc, tx) =>
      val distinct = tx.distinct
      val pairs = for (id1 <- distinct; id2 <- distinct if id1 != id2) yield (id1, id2)
      pairs.foldLeft(acc) { (acc, pair) =>
        val key = pair._1 + pair._2
        acc + (key -> (acc.getOrElse(key, 0) + 1))
      }
    }

  def buildCoOccurrenceMap4(transactions: Seq[Seq[String]]): collection.mutable.Map[String, Int] = {
    val result = mutable.Map[String, Int]()

    transactions.foreach { tx =>
      val distinct = tx.distinct

      for (id1 <- distinct;
           id2 <- distinct) {
        if (id1 != id2) {
          val key = id1 + id2;
          result.put(key, result.getOrElse(key, 0) + 1)
        }
      }
    }

    result
  }

  def buildCoOccurrenceMap5(transactions: Seq[Seq[String]]): java.util.HashMap[String, Int] = {
    val result = new java.util.HashMap[String, Int]()

    transactions.foreach { tx =>
      val distinct = tx.distinct

      for (id1 <- distinct;
           id2 <- distinct) {
        if (id1 != id2) {
          val key = id1 + id2;
          result.put(key, result.getOrDefault(key, 0) + 1)
        }
      }
    }

    result
  }

  test("coOccurrence 1") {
    val transactions = randomTransactions(6, 100000)

    val t0 = System.currentTimeMillis()

    buildCoOccurrenceMap(transactions)

    println("coOccurrence 1 %.3f sec".format((System.currentTimeMillis() - t0).toDouble / 1000.0))
  }

  test("coOccurrence 2") {
    val transactions = randomTransactions(6, 100000)

    val t0 = System.currentTimeMillis()

    buildCoOccurrenceMap2(transactions)

    println("coOccurrence 2 %.3f sec".format((System.currentTimeMillis() - t0).toDouble / 1000.0))
  }

  test("coOccurrence 3") {
    val transactions = randomTransactions(6, 100000)

    val t0 = System.currentTimeMillis()

    buildCoOccurrenceMap3(transactions)

    println("coOccurrence 3 %.3f sec".format((System.currentTimeMillis() - t0).toDouble / 1000.0))
  }

  test("coOccurrence 4") {
    val transactions = randomTransactions(6, 1000000)

    val t0 = System.currentTimeMillis()

    buildCoOccurrenceMap4(transactions)

    println("coOccurrence 4 %.3f sec".format((System.currentTimeMillis() - t0).toDouble / 1000.0))
  }

  test("coOccurrence 5") {
    val transactions = randomTransactions(6, 1000000)

    val t0 = System.currentTimeMillis()

    buildCoOccurrenceMap5(transactions)

    println("coOccurrence 5 %.3f sec".format((System.currentTimeMillis() - t0).toDouble / 1000.0))
  }

  /*
  test("Occurrence 1") {

    val transactions = randomTransactions(6, 100000)

    val t0 = System.currentTimeMillis()

    buildOccurrenceMap(transactions)

    println("Occurrence 1 %.3f sec".format((System.currentTimeMillis() - t0).toDouble / 1000.0))

  }

  test("Occurrence 2") {

    val transactions = randomTransactions(6, 100000)

    val t0 = System.currentTimeMillis()

    buildOccurrenceMap4(transactions)

    println("Occurrence 2 %.3f sec".format((System.currentTimeMillis() - t0).toDouble / 1000.0))

  }

  test("Occurrence 3") {

    val transactions = randomTransactions(6, 100000)

    val t0 = System.currentTimeMillis()

    buildOccurrenceMap2(transactions)

    println("Occurrence 3 %.3f sec".format((System.currentTimeMillis() - t0).toDouble / 1000.0))

  }
*/
}
