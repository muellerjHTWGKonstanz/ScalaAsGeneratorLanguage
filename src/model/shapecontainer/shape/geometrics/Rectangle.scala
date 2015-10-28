package model.shapecontainer.shape.geometrics

import model.Diagram
import model.shapecontainer.shape.Parsable
import model.shapecontainer.shape.geometrics.layouts.{CommonLayoutParser, RectangleEllipeLayout, CommonLayout}
import model.style.Style
import util.GeoModel

/**
 * Created by julian on 19.10.15.
 * representation of  Rectangle
 */
class Rectangle(parent:Option[GeometricModel] = None,
                commonLayout: CommonLayout,
                compartmentInfo: CompartmentInfo,
                parentOf:List[GeometricModel] = List[GeometricModel]()
                 ) extends GeometricModel(parent) with RectangleEllipeLayout with Wrapper with CompartmentInfo{

  override val style:Option[Style] = commonLayout.style
  override val position:Option[(Int, Int)] = commonLayout.position
  override val size_width: Int = commonLayout.size_width
  override val size_height: Int = commonLayout.size_height
  override var children:List[GeometricModel] = parentOf
  override val compartment_layout:CompartmentLayout = compartmentInfo.compartment_layout
  override val compartment_stretching_horizontal:Option[Boolean] = compartmentInfo.compartment_stretching_horizontal
  override val compartment_stretching_vertical:Option[Boolean] = compartmentInfo.compartment_stretching_vertical
  override val compartment_spacing:Option[Int] = compartmentInfo.compartment_spacing
  override val compartment_margin:Option[Int] = compartmentInfo.compartment_margin
  override val compartment_invisible:Option[Boolean] = compartmentInfo.compartment_invisible
  override val compartment_id:Option[String] = compartmentInfo.compartment_id

  def wraps = children
}

object Rectangle{
  /**
   * parses a GeoModel into an actual GeometricModel, in this case a Rectangle
   * @param geoModel is the sketch to parse into a GeometricModel
   * @param diagram is the document, which includes all the classHierarchy information about sevral Styles,
   * which can be used by any layout that the GeometricModel uses
   * @param parent is the parent instance that wrappes the new GeometricModel*/
  def parse(geoModel:GeoModel, diagram:Diagram, parent:Option[GeometricModel] = None): Option[Rectangle] = {
    /*mapping*/
    val commonLayout:Option[CommonLayout] = CommonLayoutParser.parse(geoModel.attributes, diagram)
    val compartmentInfo:Option[CompartmentInfo] = CompartmentInfoParser.parse(geoModel.attributes)

    if(compartmentInfo.isEmpty || commonLayout.isEmpty)
      return None

    val ret:Rectangle = new Rectangle(parent, commonLayout.get, compartmentInfo.get, List())
    ret.children = for(i <- geoModel.children)yield{i.parse(Some(ret)).get}
    Some(ret)
  }
}
