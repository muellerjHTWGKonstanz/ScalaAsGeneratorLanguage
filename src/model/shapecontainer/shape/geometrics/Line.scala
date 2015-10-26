package model.shapecontainer.shape.geometrics

import model.Diagram
import model.shapecontainer.shape.Parsable
import model.shapecontainer.shape.geometrics.layouts.{LineLayoutParser, LineLayout}
import model.style.Style
import util.GeoModel

/**
 * Created by julian on 15.10.15.
 * Representation of a simple Line
 */
class Line(parent:Option[GeometricModel]=None,
           override val style:Option[Style] = None,
           override val position:(Point, Point))
  extends GeometricModel(parent) with LineLayout

object Line{
  def parse(geoModel: GeoModel, diagram: Diagram, parent: Option[GeometricModel]): Option[Line] = {
    val lineLayout = LineLayoutParser.parse(geoModel.attributes, diagram)
    if(lineLayout.isEmpty)
      None
    else
      Some(new Line(parent, lineLayout.get.style, lineLayout.get.position))
  }
}
