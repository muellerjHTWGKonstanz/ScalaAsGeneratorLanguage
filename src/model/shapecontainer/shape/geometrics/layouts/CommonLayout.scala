package model.shapecontainer.shape.geometrics.layouts

import model.Diagram
import model.style.Style

/**
 * Created by julian on 15.10.15.
 * the commonLayout (from the Grammarsheet)
 */
trait CommonLayout extends Layout{
  val position:Option[(Int,Int)] // (x,y) Tuple
  val size_width:Int
  val size_height:Int

  /*unsafe getter!*/
  def x = position.get._1
  def y = position.get._2
}

object CommonLayoutParser {
  def parse(attributes:List[String], diagram: Diagram):Option[CommonLayout] = {
    var pos:Option[(Int,Int)] =None
    var size_w:Option[Int] = None
    var size_h:Option[Int] = None
    var styl:Option[Style] = None

    attributes.foreach {
      case x if x.matches("position \\((x=)?[0-9]+, (y=)?[0-9]+\\)") => pos = {
        val tup = "[0-9]+".r.findAllIn(x).toArray
        Some((tup(0).toInt, tup(1).toInt))
      }
      case x if x.matches("size \\((width=)?[0-9]+, (height=)?[0-9]+\\)") => {
        val tup = "[0-9]+".r.findAllIn(x).toArray
        size_w = Some(tup(0).toInt)
        size_h= Some(tup(1).toInt)
      }
      case x if x.matches("style.+") => styl = Style.parse(x, diagram)
      case x => "[CommonLayoutParser]: "+x+" was ignored "
    }

    var ret:Option[CommonLayout] = None
    if(pos.isDefined && size_w.isDefined && size_h.isDefined)
      ret = Some(new CommonLayout {
        override val position: Option[(Int, Int)] = pos
        override val size_width: Int = size_w.get
        override val size_height: Int = size_h.get
        override val style: Option[Style] = styl
      })
   ret
  }
}
