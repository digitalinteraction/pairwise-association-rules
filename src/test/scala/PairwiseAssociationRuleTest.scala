import org.scalactic.Equality
import org.scalatest._

/**
  * Created by Tim Osadchiy on 25/09/2017.
  */
class PairwiseAssociationRuleTest extends FunSuite with BeforeAndAfter {

  implicit val doubleEquality = new Equality[Double] {
    override def areEqual(a: Double, b: Any): Boolean = Math.abs(a - b.asInstanceOf[Double]) < 1e-8
  }

  private final val transactions = collection.mutable.Buffer(
    Seq("a", "b", "c", "d"),
    Seq("a", "a", "d", "e"),
    Seq("b", "d", "e")
  )

  val associationRules = PairwiseAssociationRules(transactions)

  private def combine(probabilities: Seq[Double], supports: Seq[Int], inputSize: Int, transactionsSize: Int) = {
    (1 - probabilities.foldLeft(1d)((a, b) => a * (1 - b))) * supports.map(s => s / transactionsSize.toDouble).sum / inputSize.toDouble
  }

  private def combine(probability: Double, support: Double, transactionsSize: Int) = probability * support / transactionsSize.toDouble

  private def assertTasks(tasks: Seq[(Seq[String], Map[String, Double])]) = assert(tasks.forall { task =>
    val rec = associationRules.recommend(task._1)
    val filteredRecs = rec.filter { r =>
      task._2.get(r._1).exists(_ === r._2)
    }
    filteredRecs.size == task._2.size
  })

  test("General test") {
    val firstTask = Seq(
      Seq("a") -> Map(
        "b" -> combine(1 / 2d, 1, transactions.size),
        "c" -> combine(1 / 2d, 1, transactions.size),
        "d" -> combine(2 / 2d, 2, transactions.size),
        "e" -> combine(1 / 2d, 1, transactions.size)
      ),
      Seq("d") -> Map(
        "a" -> combine(2 / 3d, 2, transactions.size),
        "b" -> combine(2 / 3d, 2, transactions.size),
        "c" -> combine(1 / 3d, 1, transactions.size),
        "e" -> combine(2 / 3d, 2, transactions.size)
      ),
      Seq("a", "d") -> Map(
        "b" -> combine(Seq(1 / 2d, 2 / 3d), Seq(1, 2), 2, transactions.size),
        "c" -> combine(Seq(1 / 2d, 1 / 3d), Seq(1, 1), 2, transactions.size),
        "e" -> combine(Seq(1 / 2d, 2 / 3d), Seq(1, 2), 2, transactions.size)
      ),
      Seq("a", "d", "f") -> Map(
        "b" -> combine(Seq(1 / 2d, 2 / 3d), Seq(1, 2), 3, transactions.size),
        "c" -> combine(Seq(1 / 2d, 1 / 3d), Seq(1, 1), 3, transactions.size),
        "e" -> combine(Seq(1 / 2d, 2 / 3d), Seq(1, 2), 3, transactions.size)
      )
    ).map(i => i._1 -> i._2)
    assertTasks(firstTask)
  }

  test("Add rules test") {
    val newTrans = Seq("c", "e", "f")
    transactions.append(newTrans)
    associationRules.addTransaction(newTrans)
    val secondTask = Seq(
      Seq("e", "f") -> Map(
        "a" -> combine(Seq(1 / 3d, 0d), Seq(1), 2, transactions.size),
        "d" -> combine(Seq(2 / 3d, 0d), Seq(2), 2, transactions.size),
        "b" -> combine(Seq(1 / 3d, 0d), Seq(1), 2, transactions.size),
        "c" -> combine(Seq(1 / 3d, 1d), Seq(1, 1), 2, transactions.size)
      )
    )
    assertTasks(secondTask)
  }

}
