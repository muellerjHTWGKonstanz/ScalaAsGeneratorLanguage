package util

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by julian on 03.11.15.
 * commonly used parsing methodes
 */
trait CommonParserMethodes extends JavaTokenParsers{
  /*basic stuff*/
  def attribute:Parser[(String, String)] = variable ~ argument <~ ",?".r ^^ {case v ~ a => (v.toString,a.toString)}
  def variable:Parser[String] = "[a-züäöA-ZÜÄÖ]+([-_][a-züäöA-ZÜÄÖ]+)*".r <~ "\\s*".r  ^^ {_.toString} //<~ "=?\\s*".r
  def argument:Parser[String] = "(([a-züäöA-ZÜÄÖ]+([-_][a-züäöA-ZÜÄÖ]+)?)|(\".*\")|([+-]?\\d+(\\.\\d+)?))".r ^^ {_.toString}
  def argument_classic: Parser[String] = """\s*\=\s*""".r ~> argument^^ { _.toString }
  def argument_advanced_explicit: Parser[String] = """\((\w+([-_]\w+)?\s*=\s*([a-zA-ZüäöÜÄÖ]+|(\".*\")|([+-]?\d+(\.\d+)?)),?[\s\n]*)+\)""".r ^^ { _.toString }
  def argument_wrapped:Parser[String] = "\\{[^\\{\\}]*\\}".r ^^ {_.toString}
  def argument_advanced_implicit: Parser[String] = """\((([a-zA-ZüäöÜÄÖ]+|(\".*\")|([+-]?\d+(\.\d+)?)),?\s*)+\)""".r ^^ { _.toString }
  def arguments: Parser[String] = argument_classic | argument_advanced_explicit | argument_advanced_implicit | argument_wrapped
  def attributeAsString: Parser[String] = variable ~ arguments ^^ { case v ~ arg => v + arg }
  def attributePair: Parser[(String, String)] = variable ~ arguments ^^ { case v ~ a => (v, a) }

  /*Some explicit usages*/
  def position:Parser[Option[(Int, Int)]] = "position\\s*\\(\\s*(x=)?".r ~> argument ~ ((",\\s*(y=)?".r ~> argument) <~")") ^^ {
    case xarg ~ yarg => Some((xarg.toInt, yarg.toInt))
    case _ => None}
  def size:Parser[Option[(Int, Int)]] = "size\\s*\\(\\s*(width=)?".r ~> argument ~ (",\\s*(height=)?".r ~> argument) <~ ")" ^^ {
    case width ~ height => Some((width.toInt, height.toInt))
    case _ => None }
  def curve:Parser[Option[(Int, Int)]] = "curve\\s*\\(\\s*(width=)?".r ~> argument ~ (",\\s*(height=)?".r ~> argument) <~ ")" ^^ {
    case width ~ height => Some((width.toInt, height.toInt))
    case _ => None }
  def idAsString:Parser[String] = "id\\s*=?\\s*".r ~> argument ^^ {case arg => arg}
  /**
   * takes a String and parses a boolean value out of it -> if string is yes|true|y
   * @param b the stringargument*/
  def matchBoolean(b: String): Boolean = b match {
    case `b` if b toLowerCase() matches "yes|true|y" => true
    //case `b` if b toLowerCase() matches("no|false|n") => false
    case _ => false
  }
}