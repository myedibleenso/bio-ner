package ner
import edu.arizona.sista.processors.Document
import edu.stanford.nlp.ling.CoreAnnotations.{TextAnnotation, TokenBeginAnnotation, TokenEndAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConverters._

/**
 * Created by gus on 8/27/14.
 */
class CoreDocument(doc:Document) {
  // make CoreSentences
  val sentences:List[CoreSentence] = (for (s <- doc.sentences) yield new CoreSentence(s)).toList

  // get CoreLabels
  val coreLabels:List[CoreLabel] = (for (s <- sentences) yield s.getCoreLabels()).toList.flatten
  val annotation:CoreMap = mkAnnotation()

  val words:List[String] = (for (c <- coreLabels) yield c.word).toList
  val tags = (for (c <- coreLabels) yield c.tag).toList

  override def toString = s"# Sentences: ${doc.sentences.length} # CoreLabels: ${coreLabels.length}"

  def updateSentenceCoreLabels() {
    /** Necessary because of limitations of current implementation where hanges to CoreDocument's
     *  CoreDocument's coreLabels are not reflected at the sentence level
     */
    var labels = coreLabels
    for (sentence <- sentences) {
      val sentenceLabels = labels.take(sentence.size).toList
      labels = labels.drop(sentence.size).toList
      sentence.coreLabels = sentenceLabels
    }
  }

  def prepareText(text:String):String = {
    //locate whitespace that precedes "punctuation"
    val punctPattern = "\\s(?=\\W)".r

    // remove unnecessary whitespace
    punctPattern.replaceAllIn(text, "").trim
  }

  def mkAnnotation():CoreMap = {
    val annotation = new Annotation("")
    // get text
    val text = prepareText((for (c <- coreLabels) yield c.word).mkString(" "))  // retrieve words from CoreLabels, join w/ whitespace, and then clean up

    // store CoreLabels
    annotation.set(classOf[TokensAnnotation], coreLabels.asJava)
    // store text
    annotation.set(classOf[TextAnnotation], text)
    // store index of first token (this is dumb)
    annotation.set(classOf[TokenBeginAnnotation], new Integer(0))
    // store index of last token (it should be able to figure this one out on its own...)
    annotation.set(classOf[TokenEndAnnotation], new Integer(coreLabels.size))
    annotation
  }
}

