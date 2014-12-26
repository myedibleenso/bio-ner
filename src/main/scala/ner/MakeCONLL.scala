package ner

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal
import edu.arizona.sista.processors.Document
import org.slf4j.LoggerFactory
import MakeCONLL.logger


/**
 * Created by gus on 7/9/14.
 */

class MakeCONLL(doc: Document) {
  //lazy val processor = new FastNLPProcessor()
  val representation = new ArrayBuffer[String]

  def retrieveRepresentation():String = {
    representation.toArray.mkString("")
  }

  def createRepresentation() {
    // Create CONLL-style representation
    val root = "root"
    val blank = "_"
    for (sentence <- doc.sentences) {
      println(sentence.words.mkString(" "))
      val deps = sentence.dependencies.get
      try {
        val sentenceArray = new ArrayBuffer[ArrayBuffer[Any]]
        for (i <- 0 until sentence.words.length) {
          // assign root if nothing retrieved...
          val (headIndex, relation) = if (!deps.getIncomingEdges(i).mkString("").isEmpty) deps.getIncomingEdges(i)(0) else (-1, root)

          // for clarity's sake...
          val tag = sentence.tags.get(i)
          val simpleTag = posMatch(tag)
          val lemma = sentence.lemmas.get(i)
          val word = sentence.words(i)
          val head = if (relation != root) sentence.words(headIndex) else root
          sentenceArray.append(ArrayBuffer(i + 1, word, lemma, simpleTag, tag, blank, headIndex + 1, relation, blank, blank))
        }
        // make sure that every line is of the same length...
        if (sentenceArray.forall(s => s.length == 10)) representation += s"${(for (s <- sentenceArray) yield s.mkString("\t")).mkString("\n")}\n\n"
      } catch {
        case NonFatal(e) => logger.info(s"WARNING: $e\ndependencies could not be generated for sentence: ${sentence.getSentenceText()}")
      }
    }
  }

  // helper method for PoS tag mappings
  def posMatch(tag:String):String = {
    //regex patterns for mappings to simplified tags
    val VERB = "MD|VB.*".r
    val NOUN = "NN.*".r  // NOUN - nouns (common and proper)
    val PRON = "(WP|PRP)".r //PRON - pronouns
    val ADJ = "JJ".r //ADJ - adjectives
    val ADV = "W?RB".r //ADV - adverbs
    val ADP = "IN".r //ADP - adpositions (prepositions and postpositions)
    val CONJ = "CC".r //CONJ - conjunctions
    val DET = "W?DT".r //DET - determiners
    val NUM = "CD".r //NUM - cardinal numbers
    val PRT = "POS".r //  PRT - particles or other function words
    val PUNCT = "[^\\w\\s].*".r //  . - punctuation

    var simpleTag = "X" //X - other: foreign words, typos, abbreviations
    tag match {
      case VERB() => simpleTag = "VERB"
      case NOUN() => simpleTag = "NOUN"
      case PRON() => simpleTag = "PRON"
      case ADJ() => simpleTag = "ADJ"
      case ADV() => simpleTag = "ADJ"
      case ADP() => simpleTag = "ADP"
      case CONJ() => simpleTag = "CONJ"
      case DET() => simpleTag = "DET"
      case NUM() => simpleTag = "NUM"
      case PRT() => simpleTag = "PRT"
      case PUNCT() => simpleTag = "."
      case _ =>
    }
    simpleTag
  }
}

object MakeCONLL {
  val logger = LoggerFactory.getLogger(classOf[MakeCONLL])
}
