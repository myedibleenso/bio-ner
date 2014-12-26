package ner

import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.stanford.nlp.ling.CoreLabel

import scala.collection.mutable.ArrayBuffer

/**
 * Created by gus on 8/24/14.
 */

// adds a method to sentence to return an Array of CoreLabels
class CoreSentence(s: Sentence) {

  var coreLabels = getCoreLabels()
  val size = coreLabels.length

  // test whether Option is null
  //def hasProperty(x:Any):Boolean = if (x != None) true else false

  override def toString = s"# CoreLabels: ${coreLabels.length}"

  def getCoreLabels():List[CoreLabel] = {

    // set relevant properties of CoreLabel; takes index of item and CoreLabel to update
    def setCoreLabelProperties(i:Int, cl:CoreLabel) {
      cl.setIndex(i)
      cl.setWord(s.words(i))
      cl.setBeginPosition(s.startOffsets(i))
      cl.setEndPosition(s.endOffsets(i))

      // set optional properties
      if (s.lemmas != None) cl.setLemma(s.lemmas.get(i))
      if (s.tags != None) cl.setTag(s.tags.get(i))
    }

    val corelabels = new ArrayBuffer[CoreLabel]

    for (i <- 0 until s.words.size) {

      val cl = new CoreLabel()
      setCoreLabelProperties(i, cl)
      corelabels.append(cl)
    }
    corelabels.toList
  }
}

