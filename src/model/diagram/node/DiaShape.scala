package model.diagram.node

import model.style.Style
import parser._

/**
 * Created by julian on 30.11.15.
 * diagrams shape definition
 */
class DiaShape(corporateStyle:Option[Style], shape:String, propertiesAndCompartments:Option[List[(String, (String, String))]], c:Cache){
  implicit val cache = c
  val referencedShape:model.shapecontainer.shape.Shape = {
    val shapesketch:ShapeSketch = shape
    /*Hier werden aus ShapeSketches endlich eigentliche Shapes!*/
    shape.toShape(corporateStyle)
  }
  val vars = propertiesAndCompartments.getOrElse(List()).filter(i => i._1 == "var").map(_._2).map(i => i._1 -> referencedShape.textMap.get(i._2)).toMap/*TODO i._1 (at second use) needs to be resolved to an Mattribute but is not possible at the moment*/
  val vals = propertiesAndCompartments.getOrElse(List()).filter(i => i._1 == "val").map(_._2).map(i => i._1 -> referencedShape.textMap.get(i._2)).toMap
  val nests = propertiesAndCompartments.getOrElse(List()).filter(i => i._1 == "nest").map(_._2).map(i => i._1 -> referencedShape.compartmentMap.get(i._2)).toMap
}
