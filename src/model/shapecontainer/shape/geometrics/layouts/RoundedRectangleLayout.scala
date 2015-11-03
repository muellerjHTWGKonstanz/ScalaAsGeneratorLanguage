package model.shapecontainer.shape.geometrics.layouts

import model.style.Style
import util.{CommonParserMethodes, GeoModel}

/**
 * Created by julian on 20.10.15.
 * representation of a RoundedRectangleLayout
 */
trait RoundedRectangleLayout extends CommonLayout{
  val curve_width:Int
  val curve_height:Int
}

object RoundedRectangleLayoutParser extends CommonParserMethodes{
  def apply(geoModel: GeoModel) = parse(geoModel)
  def parse(geoModel:GeoModel):Option[RoundedRectangleLayout]={
   val attributes = geoModel.attributes
    /*mapping*/
    val commonLayout = CommonLayoutParser.parse(geoModel)
    if(commonLayout.isEmpty)
      return None

    attributes.foreach{
      case x if x.matches("curve.+") => {
        val newCurve = parse(curve, x).get
        return Some(new RoundedRectangleLayout {
          override val style:Option[Style] = commonLayout.get.style
          override val curve_width: Int = newCurve.get._1
          override val curve_height: Int = newCurve.get._2
          override val position: Option[(Int, Int)] = commonLayout.get.position
          override val size_width: Int = commonLayout.get.size_width
          override val size_height: Int = commonLayout.get.size_height
        })
      }
      case _ =>
    }
    None
  }
}
