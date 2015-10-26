package model.shapecontainer.shape.geometrics

import model.Diagram
import model.shapecontainer.shape.geometrics.layouts.{PolyLineLayoutParser, PolyLineLayout}
import model.style.Style
import util.GeoModel

/**
 * Created by julian on 20.10.15.
 * representation of a polygon
 * Vorsicht Polygon erbt zwar von Polyline aber ein Polygon als Polyline zu benutzen ist nicht der eigentliche sinn
 * rein pragmatischm, da Polygon und PolyLine die selben Attribute haben*/

class Polygon(parent: Option[GeometricModel] = None,
              polygonLayout: PolyLineLayout,
              override var children: List[GeometricModel] = List[GeometricModel]()
               ) extends PolyLine(parent, polygonLayout) with Wrapper

object Polygon {
  def parse(geoModel: GeoModel, diagram: Diagram, parent: Option[GeometricModel]): Option[Polygon] = {
    val polygonLayout: Option[PolyLineLayout] = PolyLineLayoutParser(geoModel.attributes, diagram)
    if (polygonLayout.isEmpty)
      return None

    val ret = new Polygon(parent, polygonLayout.get)
    ret.children = for (i <- geoModel.children) yield {
      i.parse(Some(ret)).get
    }
    Some(ret)
  }
}
