package uk.ac.ncl.openlab.intake24.pairwiseAssociationRules

import org.scalatest.FunSuite

import scala.util.Random


class MemoryTest extends FunSuite {

  val ids: IndexedSeq[String] = Range(1, 3000).map("%08d".format(_))

  val random = new Random()

  def randomTransaction(length: Int): Seq[String] = Seq.fill(length)(ids(random.nextInt(ids.size)))

  def randomTransactions(length: Int, count: Int): Seq[Seq[String]] = Seq.fill(count)(randomTransaction(length))

  test("Memory benchmark") {

    val pa = PairwiseAssociationRules(None)

    val transactions = randomTransactions(6, 10000)


    val t0 = System.currentTimeMillis()

    pa.addTransactions(transactions)

    val mem1 = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val t1 = System.currentTimeMillis()

    println("Time: %.4f sec".format((t1 - t0).toDouble / 1000.0))
    println("Used memory: %.2f MB".format(mem1.toDouble / (1024.0 * 1024.0)))

  }
}
