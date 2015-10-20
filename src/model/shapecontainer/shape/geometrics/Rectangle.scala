package model.shapecontainer.shape.geometrics

import model.shapecontainer.shape.geometrics.layouts.{RectangleEllipeLayout, CommonLayout}
import model.style.Style
/**
 * Created by julian on 19.10.15.
 * repreentation of  Rectangle
 */
class Rectangle(style:Option[Style] = None,
                parent:Option[GeometricModel] = None,
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
                 ) extends GeometricModel(style, parent) with RectangleEllipeLayout with Wrapper with CompartmentInfo{

  /**
   * Secondary Constructor that allows to generate a Rectangle with only a style,
   * the children, a commonLayout and a compartmentInfo instance
   */
  def this(style:Style, parent:Option[GeometricModel] = None, children:List[GeometricModel] = List[GeometricModel](), layout:CommonLayout, compart:CompartmentInfo) =
    this(Some(style), parent, layout.position, layout.size_width, layout.size_height, children,
    compart.compartment_layout, compart.compartment_stretching_horizontal, compart.compartment_stretching_vertical,
    compart.compartment_spacing, compart.compartment_margin, compart.compartment_invisible, compart.compartment_id)
}
