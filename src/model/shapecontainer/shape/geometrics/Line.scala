package model.shapecontainer.shape.geometrics

import model.shapecontainer.shape.geometrics.layouts.LineLayout
import model.style.Style

/**
 * Created by julian on 15.10.15.
 * Representation of a simple Line
 */
class Line(style:Option[Style] = None,
           override val position:(Point, Point),
           parent:Option[GeometricModel]=None)
  extends GeometricModel(style, parent) with LineLayout
