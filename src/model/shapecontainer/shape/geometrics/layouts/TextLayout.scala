package model.shapecontainer.shape.geometrics.layouts

import model.shapecontainer.shape.geometrics.Alignment.{VAlign, HAlign}

/**
 * Created by julian on 20.10.15.
 */
trait TextLayout extends CommonLayout{
  val hAlign:HAlign
  val vAlign:VAlign
}
