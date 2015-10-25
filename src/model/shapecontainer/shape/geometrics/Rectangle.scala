package model.shapecontainer.shape.geometrics

import model.Diagram
import model.shapecontainer.shape.Parsable
import model.shapecontainer.shape.geometrics.layouts.{CommonLayoutParser, RectangleEllipeLayout, CommonLayout}
import model.style.Style
import util.GeoModel

/**
 * Created by julian on 19.10.15.
 * repreentation of  Rectangle
 */
class Rectangle(parent:Option[GeometricModel] = None,
                override val style:Option[Style] = None,
                override val position:Option[(Int, Int)],
                override val size_width: Int,
                override val size_height: Int,
                override var children:List[GeometricModel] = List[GeometricModel](),
                override val compartment_layout:CompartmentLayout,
                override val compartment_stretching_horizontal:Option[Boolean],
                override val compartment_stretching_vertical:Option[Boolean],
                override val compartment_spacing:Option[Int],
                override val compartment_margin:Option[Int],
                override val compartment_invisible:Option[Boolean],
                override val compartment_id:Option[String]
                 ) extends GeometricModel(parent) with RectangleEllipeLayout with Wrapper with CompartmentInfo{

}

object Rectangle {
  /**
   * Secondary Constructor that allows to generate a Rectangle with only a style,
   * the children, a commonLayout and a compartmentInfo instance
   */
  def apply(parent: Option[GeometricModel] = None,
            children: List[GeometricModel] = List[GeometricModel](),
            layout: CommonLayout, compart: CompartmentInfo): Rectangle =
    new Rectangle(parent, layout.style, layout.position, layout.size_width, layout.size_height, children,
      compart.compartment_layout, compart.compartment_stretching_horizontal, compart.compartment_stretching_vertical,
      compart.compartment_spacing, compart.compartment_margin, compart.compartment_invisible, compart.compartment_id)
}

object RectangleParser extends Parsable{
  /**
   * parses a GeoModel into an actual GeometricModel, in this case a Rectangle
   * @param geoModel is the sketch to parse into a GeometricModel
   * @param diagram is the document, which includes all the classHierarchy information about sevral Styles,
   * which can be used by any layout that the GeometricModel uses
   * @param parent is the parent instance that wrappes the new GeometricModel*/
  override def parse[Rectangle](geoModel:GeoModel, diagram:Diagram, parent:Option[GeometricModel] = None): Option[GeometricModel] = {
    /*mapping*/
    val commonLayout:Option[CommonLayout] = CommonLayoutParser.parse(geoModel.attributes, diagram)
    val compartmentInfo:Option[CompartmentInfo] = CompartmentInfoParser.parse(geoModel.attributes)

    if(compartmentInfo.isEmpty || commonLayout.isEmpty)
      return None

    val ret:Rectangle = Rectangle(parent, layout = commonLayout.get, compart = compartmentInfo.get)
    ret.children = for(i <- geoModel.children)yield{i.parse(Some(ret))}
    Some(ret)
  }/*TODO parse's return type cant be Option[Rectangle] only Option[GeometricModel] ?!?!?*/
}
