package model.shapecontainer.shape.geometrics

/**
 * Created by julian on 19.10.15.
 */
trait CompartmentInfo {
  val compartment_layout:CompartmentLayout
  val compartment_stretching_horizontal:Option[Boolean]
  val compartment_stretching_vertical:Option[Boolean]
  val compartment_spacing:Option[Int]
  val compartment_margin:Option[Int]
  val compartment_invisible:Option[Boolean]
  val compartment_id:Option[String]
}

abstract sealed class CompartmentLayout
  case object FIXED extends CompartmentLayout
  case object VERTICAL extends CompartmentLayout
  case object HORIZONTAL extends CompartmentLayout
  case object FIT extends CompartmentLayout
