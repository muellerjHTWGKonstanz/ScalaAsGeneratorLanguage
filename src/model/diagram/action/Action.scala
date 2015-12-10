package model.diagram.action

import util.CommonParserMethodes

class Action (val name:String, val label:String, val classsName:String, val methode:String)

object Action extends CommonParserMethodes{
  def parse(name:String, label:String, className:String, methode:String) = apply(name, label, className, methode)
  def apply(name:String, label:String, className:String, methode:String) = new Action(name, label, className, methode)
}
