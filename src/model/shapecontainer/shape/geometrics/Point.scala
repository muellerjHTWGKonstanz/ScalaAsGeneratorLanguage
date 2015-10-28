package model.shapecontainer.shape.geometrics

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by julian on 19.10.15.
 */
class Point(val x:Int, val y:Int, val curveBefore:Option[Int]=None, val curveAfter:Option[Int]=None)

object PointParser extends JavaTokenParsers{
  def apply(line:String) = parse(line)
  def parse(line:String):Option[Point] ={
    val attrOption = parse(point, line).get
    if(attrOption.isEmpty)
      return None

    val attrList = attrOption.get
    var x:Option[Int] = None
    var y:Option[Int] = None
    var curveBefore:Option[Int] = None
    var curveAfter:Option[Int] = None

    attrList.foreach{
      case tup:(String, String) if tup._1=="x" => x = Some(tup._2.toInt)
      case tup:(String, String) if tup._1=="y" => y = Some(tup._2.toInt)
      case tup:(String, String) if tup._1=="curveBefore" => curveBefore = Some(tup._2.toInt)
      case tup:(String, String) if tup._1=="curveAfter" => curveAfter = Some(tup._2.toInt)
    }
    if(x.isDefined && y.isDefined)
     Some(new Point(x.get, y.get, curveBefore, curveAfter))
    else
      None
  }

  def point = "point [\\(\\{]".r ~> rep(attribute) <~ "[\\)\\}]".r ^^ {case attr:List[(String, String)] => Some(attr)
  case _ => None}
  def attribute:Parser[(String, String)] = variable ~ argument <~ ",?".r ^^ {case v ~ a => (v.toString,a.toString)}
  def variable:Parser[String] = "[a-züäö]+([-_][a-züäö]+)?".r <~ "="  ^^ {_.toString}
  def argument = "(([a-züäö]+([-_][a-züäö]+)?)|(\".*\")|([+-]?\\d+(\\.\\d+)?))".r ^^ {_.toString}
}
