package model.diagram.node

import model.diagram.action.LocalActionGroup
import model.diagram.traits.{Container, OnCreate, Palette}
import model.style.{Style, HasStyle}

/**
 * Created by julian on 24.11.15.
 * representation of a node
 */
case class Node(name:String,
                /*TODO for type = ecore:EClass|QualifiedName*/
                shape:Option[Shape] = None,
                actionGroup:Option[LocalActionGroup] = None,
                override val style: Option[Style] = None,
                override val palletteCompartment: String = "",
                override val askFor: String = "",
                override val containmentReference: String = ""
               ) extends HasStyle with Palette with OnCreate with Container{
}


