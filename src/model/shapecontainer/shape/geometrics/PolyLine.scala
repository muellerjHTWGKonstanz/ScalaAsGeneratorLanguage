package model.shapecontainer.shape.geometrics

import model.shapecontainer.shape.geometrics.layouts.{PolyLineLayoutParser, PolyLineLayout}
import model.style.Style
import util.GeoModel

/**
 * Created by julian on 19.10.15.
 * represents a Polyline - several lines, definded by deveral Points.
 * the least amount of points is 2, the standardconstructor requires point1 and point2
 * several other points can be added in a list, or by varargs
 */
class PolyLine(parent:Option[GeometricModel]=None,
               polyLineLayout: PolyLineLayout
                ) extends GeometricModel(parent) with PolyLineLayout{
  override val style:Option[Style] = polyLineLayout.style
  override val points:List[Point] = polyLineLayout.points
}

object PolyLine{
  def apply(geoModel:GeoModel, parent:Option[GeometricModel]):Option[PolyLine] = parse(geoModel, parent)

  def parse(geoModel:GeoModel, parent:Option[GeometricModel]):Option[PolyLine] = {
    val polyLineLayout:Option[PolyLineLayout] = PolyLineLayoutParser.parse(geoModel)
    if(polyLineLayout.isEmpty)
      None
    else
      Some(new PolyLine(parent, polyLineLayout.get))
  }
}
