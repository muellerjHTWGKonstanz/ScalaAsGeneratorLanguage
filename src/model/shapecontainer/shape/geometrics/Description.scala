package model.shapecontainer.shape.geometrics

import model.shapecontainer.shape.geometrics.Alignment.{VAlign, HAlign}
import model.style.Style
import util.{Cache, CommonParserMethodes}
import model.CacheEvaluation._

/**
 * Created by julian on 03.11.15.
 * representation of a description
 */
/*TODO description is rather useless ... missing something?*/
class Description(override val id:String,
                  val style:Option[Style],
                  val hAlign:Option[HAlign],
                  val vAlign:Option[VAlign]) extends TextBody

object Description extends CommonParserMethodes{

  def parse(attrs:(String, String), parentStyle:Option[Style], hierarchyContainer: Cache):Option[Description] = {
    implicit val cache = hierarchyContainer
    /*mapping*/
    var hali:Option[HAlign] = None
    var vali:Option[VAlign] = None
    var styl:Option[Style] = Style.makeLove(hierarchyContainer, parentStyle, attrs._1)
    var id:String = ""

    val attributes = attrs._2.split("\n")
    attributes.foreach{
      case x: String if x.matches("align\\s*\\((horizontal=)?(center|left|right),\\s*(vertical=)?(top|middle|bottom)\\)") =>
        hali = Alignment.parseHAlign("(center|right|left)".r.findFirstIn(x).get)
        vali = Alignment.parseVAlign("(top|middle|bottom)".r.findFirstIn(x).get)
      case x if x.matches("id.*") => id = parse(idAsString, x).get
      case anonymousStyle:String if cache.styleHierarchy.contains(anonymousStyle) => styl = Style.makeLove(cache, styl, anonymousStyle)
      case _ =>
    }
    if(id != "")
      Some(new Description(id, styl, hali, vali))
    else
      None
  }
}
