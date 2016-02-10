package model.diagram.edge

import generators.{MreferenceMock, MclassMock}
import model.diagram.action.{ActionInclude, Action, Actions}
import model.diagram.methodes.{OnDelete, OnUpdate, OnCreate, Methodes}
import model.diagram.traits.{Container, Palette}
import model.style.Style

/**
 * Created by julian on 30.11.15.
 * representation of an Edge
 */
case class Edge(name:String,
                mcoreElement:MclassMock,
                var style: Option[Style] = None,
                            /*edge-Block*/
                connection:DiaConnection,
                from:MreferenceMock, //TODO from and to are actually mcoreAttributes
                to:MreferenceMock,//TODO from and to are actually mcoreAttributes
                override val palette:Option[String] = None,
                override val container:Option[String] = None,
                override val onCreate:Option[OnCreate] = None,
                override val onUpdate:Option[OnUpdate] = None,
                override val onDelete:Option[OnDelete] = None,
                override val actions:List[Action]      = List(),
                override val actionIncludes:Option[ActionInclude] = None
                ) extends  Palette with Container with Methodes with Actions
