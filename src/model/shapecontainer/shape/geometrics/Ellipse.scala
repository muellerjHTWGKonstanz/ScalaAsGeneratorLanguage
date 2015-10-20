package model.shapecontainer.shape.geometrics

import model.shapecontainer.shape.geometrics.layouts.CommonLayout
import model.style.Style

/**
 * Created by julian on 20.10.15.
 * representation of an Ellipse (which has the same attributes as a rectangle,
 * thats why it just extends model.shape.geometrics.Rectangle)
 * Vorsicht Ellipse erbt zwar von Rectangle, aber eine Ellipse als ein Rectangle zu benutzen ist nicht der eigentliche Sinn
 * rein pragmatisch, da Ellipse und Rectangle die selben Attribute haben
 */
class Ellipse(style:Option[Style],
              parent:Option[GeometricModel] = None,
              position:Option[(Int, Int)],
              size_width:Int,
              size_height:Int,
              children:List[GeometricModel] = List[GeometricModel](),
              compartment_layout:CompartmentLayout,
              compartment_stretching_hotizontal:Option[Boolean],
              compartment_stretching_vertical:Option[Boolean],
              compartment_spacing:Option[Int],
              compartment_margin:Option[Int],
              compartment_invisible:Option[Boolean],
              compartment_id:Option[String]
               ) extends Rectangle(style, parent, position, size_width, size_height, children,
                                   compartment_layout, compartment_stretching_hotizontal, compartment_stretching_vertical,
                                   compartment_spacing, compartment_margin, compartment_invisible, compartment_id){

  /**
   * Secondary Constructor that allows to generate a Rectangle with only a style,
   * the children, a commonLayout and a compartmentInfo instance
   */
  def this(style:Style, parent:Option[GeometricModel] = None, children:List[GeometricModel] = List[GeometricModel](), layout:CommonLayout, compart:CompartmentInfo) =
    this(Some(style), parent, layout.position, layout.size_width, layout.size_height, children,
      compart.compartment_layout, compart.compartment_stretching_horizontal, compart.compartment_stretching_vertical,
      compart.compartment_spacing, compart.compartment_margin, compart.compartment_invisible, compart.compartment_id)
}
