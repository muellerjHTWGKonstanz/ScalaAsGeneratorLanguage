package model.shapecontainer.shape.geometrics.layouts

import model.Diagram
import model.style.{StyleParser, Style}
import util.{CommonParserMethodes, GeoModel}

/**
 * Created by julian on 15.10.15.
 * the commonLayout (from the Grammarsheet)
 */
trait CommonLayout extends Layout{
  val position:Option[(Int,Int)] // (x,y) Tuple
  val size_width:Int
  val size_height:Int

  /*unsafe getter!*/
  def x = position.get._1
  def y = position.get._2
}

object CommonLayoutParser extends CommonParserMethodes{
  def parse(geoModel:GeoModel, parentStyle:Option[Style], diagram: Diagram):Option[CommonLayout] = {
    val attributes = geoModel.attributes

    /*mapping*/
    var pos:Option[(Int,Int)] =None
    var size_w:Option[Int] = None
    var size_h:Option[Int] = None
    var styl:Option[Style] = StyleParser.makeLove(diagram, parentStyle, geoModel.style) //if geoModel.style and parentstyle are defined a childStyle is created

    attributes.foreach {
      case x if x.matches("position.+") => pos = {
        val newPositoin = parse(position, x).get
        if(newPositoin isDefined)
          newPositoin
        else
          None
      }
      case x if x.matches("size.+") =>
        val newSize = parse(size, x).get
        if(newSize.isDefined){
          size_w = Some(newSize.get._1)
          size_h = Some(newSize.get._2)
        }
      case x if x.matches("style.+") & geoModel.style.isEmpty =>
        styl = StyleParser.makeLove(diagram, parentStyle, Some(StyleParser.parse(x))) //generate anonymous style
      case _ =>
    }

    var ret:Option[CommonLayout] = None
    if(size_w.isDefined && size_h.isDefined)
      ret = Some(new CommonLayout {
        override val position: Option[(Int, Int)] = pos
        override val size_width: Int = size_w.get
        override val size_height: Int = size_h.get
        override val style: Option[Style] = styl
      })
    else println("no size was given for Position in: " + geoModel.typ)//TODO replace with call to Logger
   ret
  }
}
