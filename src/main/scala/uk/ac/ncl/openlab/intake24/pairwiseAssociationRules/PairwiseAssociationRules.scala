/**
  * Created by Tim Osadchiy on 02/10/2017.
  */


package uk.ac.ncl.openlab.intake24.pairwiseAssociationRules

private object PairwiseAssociationRulesUtils {
  def pairs(l: Seq[String]): Set[(String, String)] =
    l.foldLeft(Set[(String, String)]()) {
      (acc, first) =>
        l.foldLeft(acc) {
          (acc, second) =>
            if (first != second) acc + (first -> second) else acc
        }
    }
}

case class PairwiseAssociationRulesConstructorParams(numberOfTransactions: Int, occurrences: Map[String, Int], coOccurrences: Map[String, Map[String, Int]])

class PairwiseAssociationRules(params: Option[PairwiseAssociationRulesConstructorParams]) {

  private var currentTransactionsSize = params.map(_.numberOfTransactions).getOrElse(0).toDouble

  private val occurrenceMap = collection.mutable.Map(params.map(_.occurrences)
    .getOrElse(Map()).toSeq: _*).withDefaultValue(0)

  private val coOccurrenceMap = collection.mutable.Map(params.map(_.coOccurrences).getOrElse(Map())
    .map(n => n._1 -> collection.mutable.Map(n._2.toSeq: _*)).toSeq: _*)
    .withDefaultValue(collection.mutable.Map().withDefaultValue(0))

  def recommend(items: Seq[String]): Seq[(String, Double)] = {
    val distinctItems = items.distinct
    distinctItems.flatMap {
      item => occurrenceMap.get(item).map(oc => item -> oc.toDouble)
    }.flatMap { itemNode =>
      if (itemNode._2 < 1)

      /**
        * Avoiding division by zero
        */
        None
      else coOccurrenceMap.get(itemNode._1).map { cooc =>
        cooc.map(kv => (kv._1, kv._2 / itemNode._2, itemNode._2))
      }
    }.flatten.groupBy(_._1)
      .filterNot(itemProbabilities => items.contains(itemProbabilities._1))
      .map(itemProbabilities => itemProbabilities._1 -> {
        val prob = itemProbabilities._2.map(_._2).sum
        val support = itemProbabilities._2.map(_._3).sum
        prob * support
      })
      .toSeq.sortBy(-_._2)
  }

  def addTransaction(transaction: Seq[String]) = {
    val disTrans = transaction.distinct
    currentTransactionsSize += 1
    disTrans.foreach { i =>
      occurrenceMap += (i -> (occurrenceMap(i) + 1))
    }
    PairwiseAssociationRulesUtils.pairs(disTrans).foreach { pair =>
      coOccurrenceMap += pair._1 -> (coOccurrenceMap(pair._1) +
        (pair._2 -> (coOccurrenceMap(pair._1).getOrElse(pair._2, 0) + 1)))
    }
  }

  def addTransactions(transactions: Seq[Seq[String]]) = {
    transactions.foreach(t => addTransaction(t))
  }

  def getParams() = PairwiseAssociationRulesConstructorParams(currentTransactionsSize.toInt, occurrenceMap.toMap,
    coOccurrenceMap.map(n => n._1 -> n._2.toMap).toMap)

}

object PairwiseAssociationRules {
  def apply(params: Option[PairwiseAssociationRulesConstructorParams]): PairwiseAssociationRules = new PairwiseAssociationRules(params)

  def apply(transactions: Seq[Seq[String]]): PairwiseAssociationRules = {
    val associationRulesParams = PairwiseAssociationRulesConstructorParams(
      transactions.size,
      PairwiseAssociationRules.buildOccurrenceMap(transactions),
      PairwiseAssociationRules.buildCoOccurrenceMap(transactions)
    )
    PairwiseAssociationRules(Some(associationRulesParams))
  }

  private def transactionsDistinctItems(transactions: Seq[Seq[String]]) = transactions.map(_.distinct)

  def buildOccurrenceMap(transactions: Seq[Seq[String]]) =
    transactionsDistinctItems(transactions).foldLeft(Map[String, Int]().withDefaultValue(0)) { (mp, itemSet) =>
      mp ++ itemSet.foldLeft(mp) { (mp, item) =>
        mp + (item -> (mp(item) + 1))
      }
    }

  def buildCoOccurrenceMap(transactions: Seq[Seq[String]]) =
    transactionsDistinctItems(transactions)
      .foldLeft(Map[String, Map[String, Int]]().withDefaultValue(Map().withDefaultValue(0))) {
        (occMap, transaction) =>
          PairwiseAssociationRulesUtils.pairs(transaction).foldLeft(occMap) {
            (occMap, pair) => {
              val node = occMap(pair._1)
              occMap + (pair._1 -> (node + (pair._2 -> (node(pair._2) + 1))))
            }
          }
      }

}
