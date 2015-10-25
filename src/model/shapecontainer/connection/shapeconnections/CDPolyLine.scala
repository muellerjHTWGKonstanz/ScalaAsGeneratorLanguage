package model.shapecontainer.connection.shapeconnections

import model.shapecontainer.shape.geometrics.Point
import model.shapecontainer.shape.geometrics.layouts.PolyLineLayout
import model.style.Style

/**
 * Created by julian on 20.10.15.
 */
class CDPolyLine(override val style:Option[Style]=None,
                 point1:Point,
                 point2:Point,
                 otherPoints:List[Point] = List[Point]()
                  ) extends ShapeConnection with PolyLineLayout{
  var l:List[Point] = List(point1, point2)
  override val points:List[Point] = List(point1, point2):::otherPoints
}
