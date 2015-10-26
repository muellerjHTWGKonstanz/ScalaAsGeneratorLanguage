package model.shapecontainer.shape.geometrics.layouts

import model.Diagram
import model.shapecontainer.shape.geometrics.Point
import model.style.Style

/**
 * Created by julian on 20.10.15.
 */
trait PolyLineLayout extends Layout{
  val points:List[Point]
}

object PolyLineLayoutParser{
  def apply(attributes:List[String], diagram: Diagram):Option[PolyLineLayout] = parse(attributes, diagram)
  def parse(attributes:List[String], diagram: Diagram):Option[PolyLineLayout] ={
    var collectedPoints:List[Point] = List[Point]()
    var styl:Option[Style] = None
    attributes.foreach{
      case x if x.matches("point \\((x=)?[0-9]+, (y=)?[0-9]+\\)") => {
        val tup = "[0-9]+".r.findAllIn(x).toArray
        collectedPoints = collectedPoints.::(new Point(tup(0).toInt, tup(1).toInt))
      }
      //TODO case x if x.matches("style.+") => styl = Style.parse(x, diagram)
    }
    if(collectedPoints.length > 1)
      Some(new PolyLineLayout {
        override val points: List[Point] = collectedPoints
        override val style: Option[Style] = styl
      })
    else
      None
  }
}
