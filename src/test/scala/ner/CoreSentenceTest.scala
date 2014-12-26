package ner

import org.scalatest.FunSuite
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
 * Created by gus on 9/15/14.
 */
class CoreSentenceTest extends FunSuite {
  // Test data structure
  test("getCoreLabels method works correctly") {
    lazy val processor = new FastNLPProcessor()
    val text = "I saw three clowns dancing near the pool.  I think it was around midnight"
    val doc = processor.mkDocument(text)
    //val doc = processor.annotate(text)
    for (idx <- 0 until doc.sentences.size) {
      val s = doc.sentences(idx)
      val cs = new CoreSentence(s)
      val corelabels = cs.getCoreLabels()
      println(s"sentence ${idx + 1}:")
      for (i <- 0 until corelabels.size) {
        println(s"word:\t${corelabels(i).word}")
        println(s"index:\t${corelabels(i).index}")
        println(s"tag:\t${corelabels(i).tag}")
        println(s"lemma:\t${corelabels(i).lemma}")
      }
      println("")
      assert(!cs.coreLabels.isEmpty)
    }
  }
}
