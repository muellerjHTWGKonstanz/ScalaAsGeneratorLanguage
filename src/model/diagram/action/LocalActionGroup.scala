package model.diagram.action

import model.diagram.Diagram

/**
 * Created by julian on 30.11.15.
 * representations of LocalActionGroup and ActionInclude
 */
class LocalActionGroup(val actionIncludes:List[ActionInclude],
                       val actions:List[Action]){
  /**
   * since the is no Diagram while parsing this methode has to be called when parsing a Diagram has finished
   */
  def solveOpenDependencies(diagram: Diagram) =
    actionIncludes.map(_.solveOpenDependencies(diagram))
}
object LocalActionGroup{
  def apply(actionIncludes:List[ActionInclude], actions:List[Action]) = new LocalActionGroup(actionIncludes, actions)
}


/**
 * defined in LocalActionGroup.scala because it is only used there*/
class ActionInclude(var globalActionIncludes:List[GlobalActionGroup] = List[GlobalActionGroup](), val openReferences:List[String] = List()){
  def solveOpenDependencies(diagram: Diagram) {globalActionIncludes = globalActionIncludes ::: openReferences.map(diagram.globalActionGroups(_))}
}
object ActionInclude{
  def apply(openActionReferences:List[String]) = new ActionInclude(openReferences = openActionReferences)
}
