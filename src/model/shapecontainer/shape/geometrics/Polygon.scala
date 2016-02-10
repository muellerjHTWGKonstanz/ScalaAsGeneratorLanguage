package model.shapecontainer.shape.geometrics

import model.shapecontainer.shape.geometrics.layouts.{PolyLineLayoutParser, PolyLineLayout}
import model.style.Style
import parser.{Cache, GeoModel}

/**
 * Created by julian on 20.10.15.
 * representation of a polygon
 * Vorsicht Polygon erbt zwar von Polyline aber ein Polygon als Polyline zu benutzen ist nicht der eigentliche sinn
 * rein pragmatischm, da Polygon und PolyLine die selben Attribute haben*/

sealed class Polygon private (parent: Option[GeometricModel] = None,
              polygonLayout: PolyLineLayout,
              wrapping:List[GeoModel]
               ) extends PolyLine(parent, polygonLayout) with Wrapper{
  override val children: List[GeometricModel] = wrapping.map(_.parse(Some(this), style).get)
}

object Polygon {
  def apply(geoModel: GeoModel, parent: Option[GeometricModel], parentStyle:Option[Style], hierarchyContainer:Cache)=parse(geoModel, parent, parentStyle, hierarchyContainer)
  def parse(geoModel: GeoModel, parent: Option[GeometricModel], parentStyle:Option[Style], hierarchyContainer:Cache): Option[Polygon] = {
    val polygonLayout: Option[PolyLineLayout] = PolyLineLayoutParser(geoModel, parentStyle, hierarchyContainer)
    if (polygonLayout.isEmpty)
      return None

    Some(new Polygon(parent, polygonLayout.get, geoModel.children))
  }
}
