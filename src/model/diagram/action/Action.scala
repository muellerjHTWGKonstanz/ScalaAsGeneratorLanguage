package model.diagram.action

import util.CommonParserMethodes

class Action (val name:String, val label:String, val realizedBy:String/*TODO actual JvmTypeReference?????!?!?!*/)

object Action extends CommonParserMethodes{
  def parse(name:String, label:String, realizedBy:String) = apply(name, label, realizedBy)
  def apply(name:String, label:String, realizedBy:String) = new Action(name, label, realizedBy)
}
