package model.diagram.node

import model.shapecontainer.shape.geometrics.Text
import model.shapecontainer.shape.geometrics.compartment.{Compartment, CompartmentInfo}

/**
 * Created by julian on 30.11.15.
 * diagrams shape definition
 */
class Shape(val shape: model.shapecontainer.shape.Shape,
            val vars:Map[String, Text] = Map(),/*TODO String is a Mockup for EcoreAttribute*/
            val vals:Map[String, Text] = Map(),
            val nests:Map[String, CompartmentInfo] = Map())/*TODO String is a Mockup for EcoreAttribute*/
