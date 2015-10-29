package model.style

/**
 * Created by julian on 29.10.15.
 */
sealed abstract class GradientAlignment
case object HORIZONTAL extends GradientAlignment
case object VERTICAL extends GradientAlignment

object GradientAlignment {
  def getIfValid(s: String) = {
    s match {
      case "horizontal" => Some(HORIZONTAL)
      case "vertical" => Some(VERTICAL)
      case _ => None
    }
  }
}