package model.shapecontainer.shape

import model.shapecontainer.ShapeContainerElement
import model.shapecontainer.shape.anchor.Anchor.AnchorType
import model.shapecontainer.shape.geometrics.GeometricModel
import model.style.Style

/**
 * Created by julian on 29.09.15.
 * The actual representation of a shape class,
 * will hold all the relevant attributes
 *
 * @param name = id
 * @param style = model.style.Style
 * @param shape is a list of model.shapecontainer.shape.geometrics.GeometricForm 's that can be nested inside the shape
 * TODO
 */
case class Shape( name:String = "no name",
                  style:Option[Style]                   = None,
                  size_width_min:Option[Int]            = None, /*from ShapeLayout*/
                  size_height_min:Option[Int]           = None, /*from ShapeLayout*/
                  size_width_max:Option[Int]            = None, /*from ShapeLayout*/
                  size_height_max:Option[Int]           = None, /*from ShapeLayout*/
                  stretching_horizontal:Option[Boolean] = None, /*from ShapeLayout*/
                  stretching_vertical:Option[Boolean]   = None, /*from ShapeLayout*/
                  proportional:Option[Boolean]          = None, /*from ShapeLayout*/

                  shape:Option[List[GeometricModel]]    = None,

                  description:Option[String]            = None,/*TODO strange description in grammar sheet*/

                  anchor:Option[AnchorType]             = None,

                  extendedShape:List[Shape] = List()) extends ShapeContainerElement{
  val key = hashCode
  def apply() = shape
}
