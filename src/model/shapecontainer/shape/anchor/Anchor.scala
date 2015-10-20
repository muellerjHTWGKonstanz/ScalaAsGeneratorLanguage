package model.shapecontainer.shape.anchor

/**
 * Created by julian on 19.10.15.
 */
object Anchor {

  abstract class AnchorType

  abstract class AnchorPredefined
  abstract class AnchorManual

  object Center extends AnchorPredefined
  object Corners extends AnchorPredefined

  class AnchorRelativePosition(val xoffset:Double, val yoffset:Double) extends AnchorManual
  class AnchorFixPointPosition(val x:Int, val y:Int) extends AnchorManual
}
