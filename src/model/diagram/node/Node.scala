package model.diagram.node

import model.diagram.action.{ActionInclude, Action, Actions}
import model.diagram.methodes.{Methodes, OnCreate, OnUpdate, OnDelete}
import model.diagram.traits.{Container, Palette}
import model.style.Style
import parser.{PropsAndComps, Cache}

/**
 * Created by julian on 24.11.15.
 * representation of a node
 */
case class Node(name:String,
                ecoreElement:AnyRef,/*TODO this is a mock, replace with actual ecoreElement*/
                style:Option[Style]      = None,
                                /*node-block*/
                shape:Option[DiaShape]                   = None,
                override val palette:Option[String]   = None,
                override val container:Option[AnyRef] = None,
                override val onCreate:Option[OnCreate]= None,
                override val onUpdate:Option[OnUpdate]= None,
                override val onDelete:Option[OnDelete]= None,
                override val actions:List[Action]     = List(),
                override val actionIncludes: Option[ActionInclude] = None
               ) extends Palette with Container with Methodes with Actions

