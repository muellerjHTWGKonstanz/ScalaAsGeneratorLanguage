package model.shapecontainer.connection

import model.shapecontainer.shape.Shape
import model.shapecontainer.shape.geometrics.{Text, GeometricModel}
import model.style.Style
import parser.{PlacingSketch, CommonParserMethods}

/**
 * Created by julian on 20.10.15.
 * represents a PlacingDefinition
 */
sealed class Placing private (val position_offset:Double,
                   val position_distance:Option[Int]=None,
                   val shapeCon:GeometricModel,
                   val shape:Shape){
  /*TODO placing hat laut der grammatik eigentlich keine direkte beziehung zu einer Shape lediglich eine geomtric form, welche ein CDText sein kann ???*/
  def text = shapeCon match {
    case t:Text => Some(t)
    case _ => None
  }
}

object Placing extends CommonParserMethods{
  def apply(attributes:PlacingSketch, parentStyle:Option[Style], ancestorShape:Shape) = parse(attributes, parentStyle, ancestorShape)
  def parse(attributes:PlacingSketch, parentStyle:Option[Style], ancestorShape:Shape):Placing = {
    /*mapping*/
    val tup = parse(placingPosition, attributes.position).get

    new Placing(tup._1, tup._2, attributes.shape.parse(None, parentStyle).get, ancestorShape)
  }

  def placingPosition:Parser[(Double, Option[Int])] = ("\\(\\s*offset\\s*=".r ~> argument_double) ~ (((",\\s*distance\\s*=".r ~> argument_int)?) <~ ")") ^^ {
    case offset ~ distance => (offset, distance)
  }

}


