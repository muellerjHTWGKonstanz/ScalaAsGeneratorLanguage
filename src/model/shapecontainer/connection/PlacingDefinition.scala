package model.shapecontainer.connection

import model.shapecontainer.connection.shapeconnections.ShapeConnection

/**
 * Created by julian on 20.10.15.
 * represents a PlacingDefinition
 */
class PlacingDefinition(val position_offset:Double,
                        val position_distance:Option[Int]=None,
                        val shapeCon:ShapeConnection)
