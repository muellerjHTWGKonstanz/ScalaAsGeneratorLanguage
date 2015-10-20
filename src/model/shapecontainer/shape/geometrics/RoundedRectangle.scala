package model.shapecontainer.shape.geometrics

import model.shapecontainer.shape.geometrics.layouts.{RoundedRectangleLayout, CommonLayout}
import model.style.Style
/**
 * Created by julian on 19.10.15.
 * represents a rounded rectangle
 */
class RoundedRectangle(style:Option[Style] = None,
                       parent:Option[GeometricModel] = None,
                       override val curve_width:Int, /*from RoundedRectangleLayout*/
                       override val curve_height:Int,/*from RoundedRectangleLayout*/
                       override val position: Option[(Int, Int)] = None,
                       override val size_width: Int,
                       override val size_height: Int,
                       override var children:List[GeometricModel] = List[GeometricModel]()
                        ) extends GeometricModel(style, parent) with RoundedRectangleLayout with Wrapper{

  def this(style: Style, parent:Option[GeometricModel] = None, curvewidth:Int, curveheight:Int, layout:CommonLayout, childs:List[GeometricModel]) =
    this(Some(style), parent, curvewidth, curveheight, layout.position, layout.size_width, layout.size_height, childs)

  def this(style:Style, parent:Option[GeometricModel] = None, curve_width:Int, curve_height:Int, rr:RoundedRectangle) =
    this(Some(style), parent, curve_width, curve_height, rr.position, rr.size_width, rr.size_height)
}

