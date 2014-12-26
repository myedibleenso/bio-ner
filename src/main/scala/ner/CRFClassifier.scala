package ner

import edu.stanford.nlp.ling.CoreLabel
import java.util.Properties
import java.io.File
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.List
// import scala.util.control.NonFatal
// import scala.reflect.runtime.universe._ // for type matching
import edu.stanford.nlp.ie.crf.{CRFClassifier => OriginalCRFClassifier} // rename import
import edu.stanford.nlp.ling.CoreAnnotations.AnswerAnnotation

/**
 * Created by gus on 8/24/14.
 */


class CRFClassifier {

  // create CRFClassifier
  var crf:OriginalCRFClassifier[CoreLabel] = null

  // for classification errors
  case class CustomException(e:String) extends Exception

  def makeClassifier() {
    val props = new Properties()
    props.setProperty("macro", "true") // use a generic CRF configuration
    props.setProperty("featureFactory", "edu.arizona.sista.examples.CRFFeatureFactory") // our own FF!
    //props.setProperty("l1reg", "0.1"); // for L1 regularization
    crf = new OriginalCRFClassifier(props)
  }

  def save(path: String) {
    crf.serializeClassifier(path)
  }

  def load(path: String) {
    makeClassifier()
    val file = new File(path)
    crf.loadClassifier(file)
  }

  def train(sentences: List[List[CoreLabel]]) {
    makeClassifier()
    // convert to Java lists
    val convertedSentences = (for (s <- sentences) yield s.asJava).toList.asJava
    // train crf
    crf.train(convertedSentences)
  }

  /*
    def classify[T: TypeTag](data:T) = {
      var result = null
      typeOf[T] match {
        // single sentence?
        case s if s =:= typeOf[List[CoreLabel]] => result = classifySentence(data)
        // multiple sentences?
        case ss if ss =:= typeOf[List[List[CoreLabel]]] => result = classifySentences(data)
        // something else? Throw an error
        case _ => throw CustomException(s"Can't classify ${data.getClass}")
      }
      result
    }
  */

  def classifySentence(sentence: List[CoreLabel]): Array[String] = {
    // convert list to java counterpart for classification
    val predictions = crf.classify(sentence.asJava)
    val labels = new ArrayBuffer[String]
    // convert to scala iterable
    for (p <- predictions.asScala) {
      // retrieve the label
      val label = p.get(classOf[AnswerAnnotation])
      labels += label
    }
    labels.toArray
  }

  // handle larger documents
  def classifySentences(sentences: List[List[CoreLabel]]): Array[Array[String]] = {
    val labels = new ArrayBuffer[Array[String]]
    for (s <- sentences) labels.append(classifySentence(s))
    labels.toArray
  }
}


