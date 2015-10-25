package model.shapecontainer.shape.geometrics

import model.Diagram
import model.shapecontainer.shape.Parsable
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
              style: Option[Style],
              position: Option[(Int, Int)],
              size_width: Int,
              size_height: Int,
              children: List[GeometricModel] = List[GeometricModel](),
              compartment_layout: CompartmentLayout,
              compartment_stretching_hotizontal: Option[Boolean],
              compartment_stretching_vertical: Option[Boolean],
              compartment_spacing: Option[Int],
              compartment_margin: Option[Int],
              compartment_invisible: Option[Boolean],
              compartment_id: Option[String]
               ) extends Rectangle(parent, style, position, size_width, size_height, children,
  compartment_layout, compartment_stretching_hotizontal, compartment_stretching_vertical,
  compartment_spacing, compartment_margin, compartment_invisible, compartment_id) {



}

object Ellipse extends Parsable {
  /**
   * Secondary Constructor that allows to generate a Rectangle with only a style,
   * the children, a commonLayout and a compartmentInfo instance
   */
  def apply(parent:Option[GeometricModel], children:List[GeometricModel] = List[GeometricModel](), layout: CommonLayout, compartmentInfo: CompartmentInfo) =
    new Ellipse(parent, layout.style, layout.position, layout.size_width, layout.size_height, children, compartmentInfo.compartment_layout,
    compartmentInfo.compartment_stretching_horizontal, compartmentInfo.compartment_stretching_vertical,
    compartmentInfo.compartment_spacing, compartmentInfo.compartment_margin, compartmentInfo.compartment_invisible,
    compartmentInfo.compartment_id)


  /**
   * parses a GeoModel into an actual GeometricModel, in this case a Rectangle
   * @param geoModel is the sketch to parse into a GeometricModel
   * @param diagram is the document, which includes all the classHierarchy information about sevral Styles,
   * which can be used by any layout that the GeometricModel uses
   * @param parent is the parent instance that wrappes the new GeometricModel*/
  override def parse[Ellipse](geoModel: GeoModel, diagram: Diagram, parent: Option[GeometricModel]): Option[GeometricModel] = {
    /*mapping*/
    val commonLayout: Option[CommonLayout] = CommonLayoutParser.parse(geoModel.attributes, diagram)
    val compartmentInfo: Option[CompartmentInfo] = CompartmentInfoParser.parse(geoModel.attributes)

    if (compartmentInfo.isEmpty || commonLayout.isEmpty)
      return None


    val ret = Ellipse(parent, List(), commonLayout.get, compartmentInfo.get)
    ret.children = for (i <- geoModel.children) yield {
      i.parse(Some(ret))
    }
    Some(ret)
  } /*TODO parse's return type cant be Option[Ellipse] only Option[GeometricModel] ?!?!?*/
}
