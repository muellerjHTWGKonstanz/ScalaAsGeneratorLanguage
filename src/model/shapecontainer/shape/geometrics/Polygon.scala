package model.shapecontainer.shape.geometrics

import model.shapecontainer.shape.geometrics.layouts.{PolyLineLayoutParser, PolyLineLayout}
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
  def apply(geoModel: GeoModel, parent: Option[GeometricModel])=parse(geoModel, parent)
  def parse(geoModel: GeoModel, parent: Option[GeometricModel]): Option[Polygon] = {
    val polygonLayout: Option[PolyLineLayout] = PolyLineLayoutParser(geoModel)
    if (polygonLayout.isEmpty)
      return None

    val ret = new Polygon(parent, polygonLayout.get)
    ret.children = for (i <- geoModel.children) yield {
      i.parse(Some(ret)).get
    }
    Some(ret)
  }
}
