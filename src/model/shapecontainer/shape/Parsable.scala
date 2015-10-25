package model.shapecontainer.shape

import model.Diagram
import model.shapecontainer.shape.geometrics.GeometricModel
import util.GeoModel

/**
 * Created by julian on 25.10.15.
 */
trait Parsable {
  def parse[T <: GeometricModel](geoModel:GeoModel, diagram:Diagram, p:Option[GeometricModel]):Option[T]
}
