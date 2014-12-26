package ner

import edu.arizona.sista.struct.DirectedGraph
import edu.arizona.sista.utils.StringUtils
import java.io.{PrintWriter, FilenameFilter, File}
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import edu.arizona.sista.processors.{Document, Sentence}
import scala.collection.parallel.ForkJoinTaskSupport
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor


/**
 * Created by gus on 7/7/14.
 */

class DocumentFromCONLL {
  // enforce singleton pattern
}


object DocumentFromCONLL {
  /** Convert CONLL-style annotations to a Document and then
    * process the document according to a specified view
  */

  lazy val processor = new FastNLPProcessor()

  def getCONLLSentences(file:File):Array[Array[String]] = {
    // the first line was blank ...
    //for (s <- scala.io.Source.fromFile(file.getAbsolutePath).mkString.drop(1).split("\n\n")) yield s.split("\n")

    // splits on blank lines
    for (s <- scala.io.Source.fromFile(file.getAbsolutePath).mkString.split("\n\n")) yield s.split("\n")
  }

  def mkSentence(table: Array[String]):Sentence = {

    def getOffsets(s:Array[String]):(Array[Int], Array[Int])= {
      val size = s.length
      val startOffsets = new ArrayBuffer[Int]
      val endOffsets = new ArrayBuffer[Int]
      var currentPosition = 0
      for (w <- s) {
        startOffsets += currentPosition
        endOffsets += currentPosition + w.length
        currentPosition += w.length + 1 // added for whitespace
      }
      // Adjust final values

      startOffsets(size - 1) -= 1
      endOffsets(size - 1) -= 1

      (startOffsets.toArray, endOffsets.toArray)
    }

    //Sentence-essential stuff...
    val words = new ArrayBuffer[String]
    val lemmas = new ArrayBuffer[String]
    val posTags = new ArrayBuffer[String]

    //Sentence-optional stuff...
    val nerTags = new ArrayBuffer[String]

    // make Sentence ..
    for (r <- table if !r.isEmpty) {
      val row = r.split("\t")
      words += row(0)
      lemmas += row(1)
      posTags += row(2)
    }
    val offsets = getOffsets(words.toArray)
    // use barebones constructor for sentence
    val sent = new Sentence(words.toArray, // words
      offsets._1, // startOffsets)
      offsets._2)

    // Add in some optional info (ignore the red underlines)
    sent.lemmas = Some(lemmas.toArray)
    sent.tags = Some(posTags.toArray)

    sent
  }

  def linesToSentences(lines:Array[Array[String]]):Array[Sentence] = {

    val sentences: ArrayBuffer[Sentence] = new ArrayBuffer
    for (line <- lines if !line.isEmpty && !line(0).isEmpty) sentences += mkSentence(line)

    sentences.toArray
  }

  def mkDocument(file:File):Document = {
    val lines:Array[Array[String]] = getCONLLSentences(file)

    val sentences = linesToSentences(lines)

    new Document(sentences)
  }
}
