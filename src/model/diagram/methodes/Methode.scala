package model.diagram.methodes

/**
 * Created by julian on 08.12.15.
 */
trait Methode {
  val actionBlock:Option[ActionBlock]
}

case class ActionBlock(actionIdentifier:Option[String],
                       actionGroupIdentifier:Option[String]){
  require(actionIdentifier.isDefined || actionGroupIdentifier.isDefined)
}
