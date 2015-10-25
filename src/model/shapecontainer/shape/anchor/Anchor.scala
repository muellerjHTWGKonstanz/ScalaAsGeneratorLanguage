package model.shapecontainer.shape.anchor

/**
 * Created by julian on 19.10.15.
 * representation of an Anchor, or its various types
 */
object Anchor {

  abstract class AnchorType

  abstract class AnchorPredefined extends AnchorType
    object Center extends AnchorPredefined
    object Corners extends AnchorPredefined

  abstract class AnchorManual     extends AnchorType
    class AnchorRelativePosition(val xoffset:Double, val yoffset:Double) extends AnchorManual
    class AnchorFixPointPosition(val x:Int, val y:Int) extends AnchorManual

  def getValid(input:String):Option[AnchorType] =input match {
    case line:String if line.toLowerCase == "center" => Some(Center)
    case line:String if line.toLowerCase == "corners" => Some(Corners)
    case _ => None /*TODO no example for anchordefinition...*/
  }
}
