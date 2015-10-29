package model.shapecontainer.shape.geometrics.layouts

import model.shapecontainer.shape.geometrics.{PointParser, Point}
import model.style.{StyleParser, Style}
import util.GeoModel

/**
 * Created by julian on 20.10.15.
 */
trait PolyLineLayout extends Layout{
  val points:List[Point]
}

object PolyLineLayoutParser{
  def apply(geoModel: GeoModel):Option[PolyLineLayout] = parse(geoModel)
  def parse(geoModel:GeoModel):Option[PolyLineLayout] ={
    val attributes = geoModel.attributes

    /*mapping*/
    var collectedPoints:List[Point] = List[Point]()
    var styl:Option[Style] = geoModel.style
    attributes.foreach{
      case x if x.matches("point.+") => {
        val newPoint = PointParser(x)
        if(newPoint.isDefined)collectedPoints = collectedPoints.::(newPoint.get)
      }
      case x if x.matches("style.+") & styl.isEmpty => styl = Some(StyleParser.parse(x))
    }
    if(collectedPoints.length > 1)
      Some(new PolyLineLayout {
        override val points: List[Point] = collectedPoints
        override val style: Option[Style] = styl
      })
    else
      None
  }
}
