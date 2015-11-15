package model.shapecontainer.shape.geometrics

import model.Diagram
import model.shapecontainer.shape.geometrics.layouts.{CommonLayoutParser, CommonLayout}
import model.style.Style
import util.GeoModel

/**
 * Created by julian on 20.10.15.
 * representation of an Ellipse (which has the same attributes as a rectangle,
 * thats why it just extends model.shape.geometrics.Rectangle)
 * Vorsicht Ellipse erbt zwar von Rectangle, aber eine Ellipse als ein Rectangle zu benutzen ist nicht der eigentliche Sinn
 * rein pragmatisch, da Ellipse und Rectangle die selben Attribute haben
 */
class Ellipse(parent: Option[GeometricModel] = None,
              commonLayout: CommonLayout,
              compartmentInfo:Option[CompartmentInfo],
              parentOf: List[GeometricModel] = List[GeometricModel]()
               ) extends Rectangle(parent, commonLayout, compartmentInfo, parentOf){
  override val style:Option[Style] = commonLayout.style
  override val position:Option[(Int, Int)] = commonLayout.position
  override val size_width: Int = commonLayout.size_width
  override val size_height: Int = commonLayout.size_height
  override val compartment_layout:Option[CompartmentLayout]= if(compartmentInfo isDefined)compartmentInfo.get.compartment_layout else None
  override val compartment_stretching_horizontal: Option[Boolean] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_stretching_horizontal else None
  override val compartment_stretching_vertical: Option[Boolean] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_stretching_vertical else None
  override val compartment_spacing: Option[Int] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_spacing else None
  override val compartment_margin: Option[Int] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_margin else None
  override val compartment_invisible: Option[Boolean] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_invisible else None
  override val compartment_id: Option[String] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_id else None
}

object Ellipse {
  /**
   * parses a GeoModel into an actual GeometricModel, in this case a Rectangle
   * @param geoModel is the sketch to parse into a GeometricModel
   * @param parent is the parent instance that wraps the new GeometricModel*/
  def apply(geoModel: GeoModel, parent: Option[GeometricModel], parentStyle:Option[Style], diagram:Diagram)= parse(geoModel, parent, parentStyle, diagram)
  def parse(geoModel: GeoModel, parent: Option[GeometricModel], parentStyle:Option[Style], diagram:Diagram): Option[Ellipse] = {
    /*mapping*/
    val commonLayout: Option[CommonLayout] = CommonLayoutParser.parse(geoModel, parentStyle, diagram)
    val compartmentInfo: Option[CompartmentInfo] = CompartmentInfoParser.parse(geoModel.attributes)

    if (commonLayout.isEmpty)
      return None

    val ret = new Ellipse(parent, commonLayout.get, compartmentInfo)
    ret.children = for (i <- geoModel.children) yield {i.parse(Some(ret), ret.style).get}
    Some(ret)
  }
}
