package model

import model.diagram.Diagram
import model.diagram.action.{ActionGroup, Action}
import model.shapecontainer.connection.Connection
import model.shapecontainer.shape.Shape
import model.style.Style

/**
 * Created by julian on 24.09.15.
 * representation of a container element for shapes and styles
 */
case class Cashe(var diagrams:Map[String, Diagram] = Map[String, Diagram](),
                 styleHierarchy: ClassHierarchy[Style] = new ClassHierarchy[Style](new Style(name = "rootStyle")),
                 shapeHierarchy: ClassHierarchy[Shape] = new ClassHierarchy[Shape](new Shape(name = "rootShape")),
                 var connections:Map[String, Connection] = Map[String, Connection](),
                 var actions:Map[String, Action] = Map[String, Action](),
                 var actionGroups:Map[String, ActionGroup] = Map[String, ActionGroup]()){
  def +(diagram: Diagram) = diagrams += diagram.name -> diagram
  def +(connection:Connection) = connections += connection.name -> connection
  def +(action:Action) = actions += action.name -> action
  def +(actionGroup:ActionGroup) = actionGroups += actionGroup.name -> actionGroup
}

