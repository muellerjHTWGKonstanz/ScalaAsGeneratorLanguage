package model.shapecontainer.connection.shapeconnections

import model.shapecontainer.shape.geometrics.Point
import model.shapecontainer.shape.geometrics.layouts.LineLayout
import model.style.Style

/**
 * All the possible CDElements
 * */

class CDLine(override val style:Option[Style]=None,
             override val points:(Point, Point)
              ) extends ShapeConnection with LineLayout
