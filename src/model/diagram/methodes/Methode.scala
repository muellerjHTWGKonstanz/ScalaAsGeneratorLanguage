package model.diagram.methodes

import model.diagram.action.{ActionGroup, Action}

/**
 * Created by julian on 08.12.15.
 */
trait Methode {
  val actionBlock:Option[ActionBlock]
}

case class ActionBlock(action:List[Action], actionGroup:List[ActionGroup]){
  require(action.nonEmpty || actionGroup.nonEmpty)
}
