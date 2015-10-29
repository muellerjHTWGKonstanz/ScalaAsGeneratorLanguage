package model.shapecontainer.shape.geometrics.layouts

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
  def parse(geoModel:GeoModel):Option[CommonLayout] = {
    val attributes = geoModel.attributes

    /*mapping*/
    var pos:Option[(Int,Int)] =None
    var size_w:Option[Int] = None
    var size_h:Option[Int] = None
    var styl:Option[Style] = geoModel.style

    attributes.foreach {
      case x if x.matches("position.+") => pos = {
        parse(position, x).get
      }
      case x if x.matches("size.+") => {
        val newSize = parse(size, x).get
        if(newSize.isDefined){
          size_w = Some(newSize.get._1)
          size_h = Some(newSize.get._2)
        }
      }
      case x if x.matches("style.+") & styl.isEmpty => styl = Some(StyleParser.parse(x))
      case x => println("[CommonLayoutParser]: "+x+" was ignored")
    }

    var ret:Option[CommonLayout] = None
    if(size_w.isDefined && size_h.isDefined)
      ret = Some(new CommonLayout {
        override val position: Option[(Int, Int)] = pos
        override val size_width: Int = size_w.get
        override val size_height: Int = size_h.get
        override val style: Option[Style] = styl
      })
   ret
  }
}
