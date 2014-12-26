package ner

import java.io.{InputStream, File}
import edu.arizona.sista.processors.Document
import edu.stanford.nlp.ling.CoreLabel
import java.net.URL

/**
 * Created by gus on 9/15/14.
 */
class ProteinComplexNER {

}

object ProteinComplexNER {

  var rulesFile:URL = getClass.getResource("/bionlp/complex_rules.txt")
  var genericRulesFile = getClass.getResource("/bionlp/complex_rules_generic.txt")
  var outputFile:File = null
  // var rulesFile:File = new File(s"src/main/resources/bionlp/complex_rules.txt")
//  rulesFile.getClass
 // println(s"rulesFile:\t${rulesFile.getClass}")

  def writeDocument(doc:CoreDocument, outfile:File) {
    val out = new java.io.PrintWriter(outfile)

    // attempt to write data to disk
    try {

      for (sentence <- doc.sentences) {
        val coreLabels = sentence.coreLabels
        for (row <- coreLabels) out.write(s"${row.word}\t${row.lemma}\t${row.tag}\t${if (row.ner != null) row.ner else "O"}\n")

        // add a blank line between sentences
        out.write("\n")
      }

      //for (cl <- doc.coreLabels) out.write(s"${cl.word}\t${cl.lemma}\t${cl.tag}\t${if (cl.ner != null) cl.ner else "O"}\n")
      println(s"${outfile.getName} written!")
    } finally out.close()
  }

  def main(args: Array[String]) {

    def onError() {
      println(s"${Console.RED}ERROR: not a valid filename.${Console.RESET}")
      println(s"USAGE:\t${Console.BOLD}./pc-ner path/to/file.tags${Console.RESET}")
      println(s"optionally...")
      println(s"USAGE:\t${Console.BOLD}./pc-ner path/to/file.tags where/you/want/your/output${Console.RESET}")
      System.exit(1)
    }

    // present usage instructions and exit if no arguments given
    if (args.isEmpty) onError()

    val inFile = new File(args(0))

    // see if an output file was specified
    outputFile =  if (args.length > 1) new File(args(1)) else new File(inFile.getParentFile, inFile.getName.take(1 + inFile.getName.lastIndexOf(".")) + "ner")

    // see if there's a problem
    if (!inFile.exists || !inFile.getName.endsWith(".tags")) {
      onError()
    }

    // generate Document from CONLL-style file
    val doc = DocumentFromCONLL.mkDocument(inFile)

    // set NER labels

    RegexNER.rulesFile = rulesFile
    RegexNER.genericRulesFile = genericRulesFile

    // TODO: test if processing CoreLabels for each sentence is faster than processing all at once
    val coreDoc = RegexNER.labelDocument(doc)

    // write annotations to disk

    writeDocument(coreDoc, outputFile)
  }
}
