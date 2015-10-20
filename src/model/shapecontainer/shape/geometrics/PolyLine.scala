package model.shapecontainer.shape.geometrics

import model.shapecontainer.shape.geometrics.layouts.PolyLineLayout
import model.style.Style
/**
 * Created by julian on 19.10.15.
 * represents a Polyline - several lines, definded by deveral Points.
 * the least amount of points is 2, the standardconstructor requires point1 and point2
 * several other points can be added in a list, or by varargs
 */
class PolyLine(style:Option[Style] = None,
               parent:Option[GeometricModel]=None,
               point1: Point,
               point2: Point,
               otherPoints:List[Point] = List[Point]()
                ) extends GeometricModel(style, parent) with PolyLineLayout{

  override val points:List[Point] = List(point1, point2):::otherPoints

  def this(style:Option[Style], parent:Option[GeometricModel], severalPoints:Point*) =
    this(style, parent, severalPoints(0), severalPoints(1), severalPoints.toList)

  def this(style:Style, parent:Option[GeometricModel], severalPoints:Point*) =
    this(Some(style), parent, severalPoints(0), severalPoints(1), severalPoints.toList)
}
