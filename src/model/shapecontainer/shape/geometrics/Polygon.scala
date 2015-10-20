package model.shapecontainer.shape.geometrics

import model.style.Style

/**
 * Created by julian on 20.10.15.
 * representation of a polygon
 *Vorsicht Polygon erbt zwar von Polyline aber ein Polygon als Polyline zu benutzen ist nicht der eigentliche sinn
 * rein pragmatischm, da Polygon und PolyLine die selben Attribute haben*/

class Polygon(style:Option[Style] = None,
              parent:Option[GeometricModel] = None,
              point1:Point,
              point2:Point,
              otherPoints:List[Point],
              override var children:List[GeometricModel] = List[GeometricModel]()
               )extends PolyLine(style, parent, point1, point2, otherPoints) with Wrapper{

  def this(style:Option[Style], parent:Option[GeometricModel], severalPoints:Point*) =  this(style, parent, severalPoints(0), severalPoints(1), severalPoints.toList)
  def this(style:Style, parent:Option[GeometricModel], severalPoints:Point*) = this(Some(style), parent, severalPoints(0), severalPoints(1), severalPoints.toList)
}
