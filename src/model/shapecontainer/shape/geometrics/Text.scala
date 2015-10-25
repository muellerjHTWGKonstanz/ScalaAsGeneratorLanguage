package model.shapecontainer.shape.geometrics

import model.shapecontainer.shape.geometrics.layouts.{TextLayout, CommonLayout}
import model.style.Style
import Alignment._
/**
 * Created by julian on 19.10.15.
 * representation of a text-element
 */
class Text(override val style:Option[Style] = None,
            parent:Option[GeometricModel] = None,
            id:String = "", /*textBody (GrammarSheet)*/
            override val position:Option[(Int, Int)]=None,
            override val size_width:Int,
            override val size_height:Int,
            override val hAlign:HAlign,
            override val vAlign:VAlign) extends GeometricModel(parent) with TextLayout

abstract class TextType
  case object DefaultText extends TextType
  case object Multiline extends TextType
