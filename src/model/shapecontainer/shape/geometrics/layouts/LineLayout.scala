package model.shapecontainer.shape.geometrics.layouts

import model.Diagram
import model.shapecontainer.shape.geometrics.{PointParser, Point}
import model.style.{StyleParser, Style}
import util.GeoModel

/**
 * Created by julian on 20.10.15.
 * representation of a LineLayout
 */
trait LineLayout extends Layout{
  val position:(Point, Point)
}

object LineLayoutParser{
  def parse(geoModel:GeoModel):Option[LineLayout]={
    val attributes = geoModel.attributes

    /*mapping*/
    var point1:Option[Point] = None
    var point2:Option[Point] = None
    var styl:Option[Style] = geoModel.style
    attributes.foreach {
      case x if x.matches("point.+") =>{
        if(point1.isEmpty)
          point1 = PointParser(x)
        else {
          point2 = PointParser(x)
        }
      }
      case x if x.matches("style.+") & styl.isEmpty => styl = Some(StyleParser.parse(x))
    }
    if(point1.isDefined && point2.isDefined)
      Some(new LineLayout {
      override val style = styl
      override val position: (Point, Point) = (point1.get, point2.get)
    })
    else
      None
  }
}
