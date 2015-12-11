package model.diagram.node

import model.shapecontainer.shape.geometrics.{CompartmentInfo, Text}

/**
 * Created by julian on 30.11.15.
 * diagrams shape definition
 */
case class Shape(shape: model.shapecontainer.shape.Shape,
                 vars:Map[String, Text] = Map(),/*TODO String is a Mockup for EcoreAttribute*/
                 vals:Map[String, Text] = Map(),
                 nests:Map[String, CompartmentInfo] = Map())/*TODO String is a Mockup for EcoreAttribute*/
