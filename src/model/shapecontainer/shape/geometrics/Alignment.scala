package model.shapecontainer.shape.geometrics

/**
 * Created by julian on 19.10.15.
 */
object Alignment {
  abstract class HAlign
  case object LEFT extends HAlign
  case object CENTER extends HAlign
  case object RIGHT extends HAlign

  abstract class VAlign
  case object TOP extends VAlign
  case object MIDDLE extends VAlign
  case object BOTTOM extends VAlign
}
