package model.shapecontainer.shape.geometrics.layouts

import model.Diagram
import model.shapecontainer.shape.geometrics.Point
import model.style.Style

/**
 * Created by julian on 20.10.15.
 * representation of a LineLayout
 */
trait LineLayout extends Layout{
  val position:(Point, Point)
}

object LineLayoutParser{
  def parse(attributes:List[String], diagram: Diagram):Option[LineLayout]={
    var point1:Option[Point] = None
    var point2:Option[Point] = None
    var styl:Option[Style] = None
    attributes.foreach {
      case x if x.matches("point \\((x=)?[0-9]+\\)") =>{
        val tup = "[0-9]+".r.findAllIn(x).toArray
        if(point1.isEmpty)
          point1 = Some(new Point(tup(0).toInt, tup(1).toInt))
        else {
          point2 = Some(new Point(tup(0).toInt, tup(1).toInt))
        }
      }
      //TODO case x if x.matches("style.+") => styl = Style.parse(x, diagram)
    }
    if(point1.isDefined && point2.isDefined)
      Some(new LineLayout {
      override val style = styl
      override val position: (Point, Point) = (point1.get, point2.get)
    })
    else
      None
  }
}
