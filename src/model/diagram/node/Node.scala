package model.diagram.node

import model.diagram.action.{ActionInclude, Action, Actions, LocalActionGroup}
import model.diagram.methodes.{Methodes, OnCreate, OnUpdate, OnDelete}
import model.diagram.traits.{Container, Palette}
import model.style.{Style, HasStyle}

/**
 * Created by julian on 24.11.15.
 * representation of a node
 */
case class Node(name:String,
                ecoreElement:AnyRef,/*TODO this is a mock, replace with actual ecoreElement*/
                override val style:Option[Style]      = None,
                                /*node-block*/
                shape:Option[Shape]                   = None,
                override val palette:Option[String]   = None,
                override val container:Option[String] = None,
                override val onCreate:Option[OnCreate]= None,
                override val onUpdate:Option[OnUpdate]= None,
                override val onDelete:Option[OnDelete]= None,
                override val actions:List[Action]     = None,
                override val actionIncludes: Option[ActionInclude] = List()
               ) extends HasStyle with Palette with Container with Methodes with Actions
/*TODO in constructor resolve open dependencies for example of the actionIncludes(are strings should be a list of actiongroups)*/


