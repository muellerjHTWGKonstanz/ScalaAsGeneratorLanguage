package model.shapecontainer.shape.geometrics.layouts

import model.Cache
import model.shapecontainer.shape.geometrics.{PointParser, Point}
import model.style.Style
import util.GeoModel

/**
 * Created by julian on 20.10.15.
 * representation of a LineLayout
 */
trait LineLayout extends Layout{
  val position:(Point, Point)
}

object LineLayoutParser{
  def parse(geoModel:GeoModel, parentStyle:Option[Style], hierarchyContainer: Cache):Option[LineLayout]={
    val attributes = geoModel.attributes

    /*mapping*/
    var point1:Option[Point] = None
    var point2:Option[Point] = None
    var styl:Option[Style] = Style.makeLove(hierarchyContainer, parentStyle, geoModel.style)
    attributes.foreach {
      case x if x.matches("point.+") =>
        if(point1.isEmpty)
          point1 = PointParser(x)
        else {
          point2 = PointParser(x)
        }
      case x if x.matches("style.+") =>
        styl = Style.makeLove(hierarchyContainer, styl, Some(Style(x, hierarchyContainer)))
      case _ =>
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
