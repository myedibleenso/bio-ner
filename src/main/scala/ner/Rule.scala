package ner

/**
 * Created by gus on 9/8/14.
 */
class Rule(val rule:String, val label:String) {

  def this(rl:(String,String)) {
    this(rl._1, rl._2)
  }
  def this(rl:Array[String]) {
    this(rl(0), rl(1))
  }
  override def toString = s"$rule => $label"
}
