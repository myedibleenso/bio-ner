package ner

import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.ling.tokensregex._
import scala.collection.JavaConverters._
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

import scala.collection.mutable.ArrayBuffer
import edu.arizona.sista.processors.Document
import scala.annotation.tailrec
import java.io.{InputStream, File}
import java.util.regex.Pattern
import scala.io.Source
import java.net.URL

/**
 * Created by gus on 8/24/14.
 */
class RegexNER {

}

case class NERMatch(startEnd: (Int, Int), label: String) {
  val start = startEnd._1
  val end = startEnd._2
  val length = end - start
}

object RegexNER {

  var rulesFile:URL = null
  var genericRulesFile:URL = null

  //File = new File(ClassLoader.getSystemResource("resources/bionlp/complex_rules.txt").toURI)

  def demo(args: Array[String]) {
    val processor = new FastNLPProcessor()
    val text = "Elephants are not interested salmon.  However, bears are interested in salmon."
    val doc = new CoreDocument(processor.annotate(text))
    val cl = doc.coreLabels
    // val annotation = doc.annotation
    //StupidRuleMatch(coreLabels)
    // TODO: Display stupid rules output

    val rules = getRules()
    smartRuleMatch(cl, rules)
    for (c <- cl) println(s"word:\t${c.word}\t\tner:\t${c.ner}")
  }

  def reviewLabels(doc:CoreDocument) {
    for (s <- doc.sentences) {
      for (cl <- s.coreLabels if cl.ner != null) println(s"${cl.word}\t${cl.lemma}\t${cl.tag}\t${cl.ner}")
    }
  }

  def labelDocument(doc:Document):CoreDocument = {
    val coreDoc = new CoreDocument(doc)

    // Defines the rule application procedure
    def parallelRuleApplication(rules:Array[Rule]) {
      var sentenceNum = 0
      for (s <- coreDoc.sentences.par) {
        smartRuleMatch(s.coreLabels, rules)
        // To keep track of progress ...
        sentenceNum += 1
        if (sentenceNum % 50 == 0) println(s"Finished evaluating ${sentenceNum} out of ${coreDoc.sentences.size} sentences...")
      }
    }

    val rules = getRules()
    // annotate relevant coreLabels with detected NER labels
    parallelRuleApplication(rules)
    // Do we have generic rules to apply?
    if (genericRulesFile != null) {
      val genericRules = getRules(genericRulesFile)
      if (!genericRules.isEmpty) parallelRuleApplication(genericRules)
    }

    println("\nUpdates to CoreLabels:")
    reviewLabels(coreDoc)

    coreDoc
  }


  def getRules(file:URL = rulesFile):Array[Rule] = {
    // retrieve valid lines from rules file
    val rules = (for (line <- scala.io.Source.fromURL(file).getLines() if line.startsWith("(")) yield new Rule(line.split('\t'))).toArray
    val fname = rulesFile.getFile.drop(rulesFile.getFile.lastIndexOf("/") + 1)
    println(s"Searching for NERs using ${rules.length} rules from $fname ...")
    rules
  }

  // optional tokens are specified as /somestuff/?
  def example(cl: List[CoreLabel]) {
    // val rules: Array[Rule] = new Array(new Rule(new Array("(/interested/ /at/? /salmon/)", "FISH")), new Rule(new Array("(/bears/ /are/)", "BEAR")))
    // read rules from file
    val rules = getRules()
    smartRuleMatch(cl, rules)
    for (c <- cl) println(s"word:\t${c.word}\t\tner:\t${c.ner}")
  }

  def NERisEmpty(tokens: List[CoreLabel], nerMatch: NERMatch):Boolean = {
    // checks to see if an NER label has already been assigned for any CoreLabel in the matched span
    for (i <- nerMatch.start until nerMatch.end) { // don't go to the end of the match
    if (tokens(i).ner != null) return false
  }
  true
  }

  def setNER(tokens: List[CoreLabel], nerMatch: NERMatch) {
    /*
    Used to set ner field in matching CoreLabels
     */

    val matchedText = (for (i <- nerMatch.start until nerMatch.end) yield tokens(i).word).mkString(" ")
    println(s"Assigning ${nerMatch.label} to \'$matchedText\'")
    val start = nerMatch.start
    val end = nerMatch.end
    val label = nerMatch.label
    // set beginning label
    tokens(start).setNER(s"B-$label")
    //println(s"${tokens(start).word}\t${tokens(start).ner}")
    if (nerMatch.length > 1) { // This test is critical!
      // set inside labels
      for (i <- start + 1 until end) {
        tokens(i).setNER(s"I-$label")
        println(s"${tokens(i).word}\t${tokens(i).ner}")
      }
    }
  }

  @tailrec def smartRuleMatch(coreLabels: List[CoreLabel], rules: Array[Rule]) {
    // TODO: speed this up

    var ruleCount = 0

    // keep track of earliest match (and its length)
    var earliestMatch = coreLabels.size
    var earliestLength = 0

    // keep track of matching rules
    val filteredRules = new ArrayBuffer[Rule]

    // test start of match interval
    var bestMatch: NERMatch = null
    for (rule <- rules.par) {
      //val rule = rules(i)
      //if (i % 500 == 0) println(s"Checking rule $i of ${rules.length}...")
      val m = matchRule(rule, coreLabels)
      // do we have a match?
      if (m != null) {
        println(s"Found a potential match at ${m.start}...")
        // keep the rule
        filteredRules += rule

        // if our match's start is the same as our current earliest...
        if (m.start == earliestMatch) {
          // is this one longer?  Are all CoreLabels in the matched span without a NER label? (<- only really matters when applying generic rules)
          if ((m.length > earliestLength) && NERisEmpty(coreLabels, m)) {
            println(s"Longer match now at ${m.start}...")
            // update our length and best match
            earliestLength = m.length
            bestMatch = m
          }

          // if our match's start is less that our current earliest...
          // AND
          // All CoreLabels in the matched span lack an NER label (<- only really matters when applying generic rules)
        } else if (m.start < earliestMatch  &&  NERisEmpty(coreLabels, m)) {
          println(s"Best match now at ${m.start}...")
          // update everything
          earliestMatch = m.start
          earliestLength = m.length
          bestMatch = m
        }

      // moved stuff from here
      }
    }
    // apply earliest, longest rule
    if (bestMatch != null) setNER(coreLabels, bestMatch)

    // we don't need to check all the core labels next time...
    val newcl = coreLabels.drop(earliestMatch + 1)

    // Do we have any CoreLabels left? Did we match more than one rule?
    if ((newcl.size > 0) & (filteredRules.size > 1)) smartRuleMatch(newcl, filteredRules.toArray)
  }

  def matchRule(ruleLabel:Rule, cl: List[CoreLabel]): NERMatch = {
    val rule = ruleLabel.rule
    val label = ruleLabel.label

    // prepare rule
    var nerMatch: NERMatch = null

    // configure environment for case insensitive matching
    //This was messing up the behavior of optional matches (ex: /complex/? was also matching /that/)...WTF?
    //val env = TokenSequencePattern.getNewEnv
    //env.setDefaultStringPatternFlags(Pattern.CASE_INSENSITIVE)

    // compile rule
    val pattern = TokenSequencePattern.compile(rule)


    // create matcher object
    val matcher = pattern.getMatcher(cl.asJava)
    // not sure what this is doing...
    matcher.matches()
    // do we have a match?
    if (matcher.find()) {
      val startEnd = (matcher.getInterval.getBegin.toInt, matcher.getInterval.getEnd.toInt)
      nerMatch = NERMatch(startEnd, label)
    }
    nerMatch
  }


  def StupidRuleMatch(coreLabels: List[CoreLabel], rule: String = "(/interested/ /.*/ /salmon/)", label: String = "FISH") {
    val pattern = TokenSequencePattern.compile(rule)
    val matcher = pattern.getMatcher(coreLabels.asJava)
    matcher.matches()

    // find all rule matches
    while (matcher.find()) {
      val startEnd = (matcher.getInterval.getBegin.toInt, matcher.getInterval.getEnd.toInt)
      setNER(coreLabels, NERMatch(startEnd, label))
    }
  }
}