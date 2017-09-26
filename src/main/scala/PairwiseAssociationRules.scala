/**
  * Created by Tim Osadchiy on 25/09/2017.
  */
class PairwiseAssociationRules(transactions: Seq[Seq[String]]) {

  private val uniqueTransactions = collection.mutable.Seq(transactions.map(_.distinct): _*)

  private var currentTransactionsSize = uniqueTransactions.size.toDouble

  private val occurrenceMap = getOccurrenceMap(uniqueTransactions)

  private val coOccurrenceMap = getCoOccurrenceMap(uniqueTransactions)

  private def getCoOccurrenceMap(transactions: Seq[Seq[String]]) = {
    val mut = transactions.foldLeft(Map[String, Map[String, Int]]().withDefaultValue(Map().withDefaultValue(0))) {
      (occMap, transaction) =>
        pairs(transaction).foldLeft(occMap) {
          (occMap, pair) => {
            val node = occMap(pair._1)
            occMap + (pair._1 -> (node + (pair._2 -> (node(pair._2) + 1))))
          }
        }
    }
    collection.mutable.Map(mut.map(n => n._1 -> collection.mutable.Map(n._2.toSeq: _*)).toSeq: _*)
      .withDefaultValue(collection.mutable.Map().withDefaultValue(0))
  }

  private def getOccurrenceMap(transactions: Seq[Seq[String]]) = {
    val mut = transactions.foldLeft(Map[String, Int]().withDefaultValue(0)) { (mp, itemSet) =>
      mp ++ itemSet.foldLeft(mp) { (mp, item) =>
        mp + (item -> (mp(item) + 1))
      }
    }
    collection.mutable.Map(mut.toSeq: _*).withDefaultValue(0)
  }

  private def pairs(l: Seq[String]): Set[(String, String)] =
    l.foldLeft(Set[(String, String)]()) {
      (acc, first) =>
        l.foldLeft(acc) {
          (acc, second) =>
            if (first != second) acc + (first -> second) else acc
        }
    }

  def recommend(items: Seq[String]) =
    items.distinct.flatMap {
      item => occurrenceMap.get(item).map(oc => item -> oc.toDouble)
    }.flatMap { itemNode =>
      coOccurrenceMap.get(itemNode._1).map { cooc =>
        cooc.map(kv => (kv._1, kv._2 / itemNode._2, kv._2 / currentTransactionsSize))
      }
    }.flatten.groupBy(_._1)
      .filterNot(itemProbabilities => items.contains(itemProbabilities._1))
      .map(itemProbabilities => itemProbabilities._1 -> {
        val prob = 1 - itemProbabilities._2.map(_._2).foldLeft(1d)((a, b) => a * (1 - b))
        val support = itemProbabilities._2.map(_._3).sum / items.distinct.size
        prob * support
      })
      .toSeq.sortBy(-_._2)

  def addTransaction(transaction: Seq[String]) = {
    val disTrans = transaction.distinct
    currentTransactionsSize += 1
    disTrans.foreach { i =>
      occurrenceMap += (i -> (occurrenceMap(i) + 1))
    }
    pairs(disTrans).foreach { pair =>
      coOccurrenceMap += pair._1 -> (coOccurrenceMap(pair._1) +
        (pair._2 -> (coOccurrenceMap(pair._1).getOrElse(pair._2, 0) + 1)))
    }
  }

}

object PairwiseAssociationRules {
  def apply(items: Seq[Seq[String]]) = new PairwiseAssociationRules(items)
}
