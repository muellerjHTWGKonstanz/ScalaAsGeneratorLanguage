package model

/**
 * Created by julian on 24.09.15.
 */

sealed class ClassHierarchy[T <% { def toString :String; val name:String}](rootClass:T){

val root = Node(rootClass)
  var nodeView:Map[String, Node] = Map(root.data.name -> root)

  /*several apply methods to simplify acces on elements*/
  def apply(parent:Node, className:T) = parent inheritedBy className
  def apply(parent:T, className:T) = nodeView(parent.name) inheritedBy className
  def apply(parent:String, className:T) = nodeView(parent) inheritedBy className
  def apply(className:T)=nodeView(className.name)
  def apply(className:String) = nodeView(className)
  override def toString = root toString

  def newBaseClass(className:T) = root inheritedBy className
  def get(styleName:T) = nodeView(styleName.name)
  def get(styleName:String) = nodeView(styleName)
  def setRelation(parent:T, child:T) = nodeView(parent.name) inheritedBy child
  def contains(className:String):Boolean = nodeView.contains(className)

  sealed case class Node(data: T,
                         var parents:List[Node] = List(),
                         var children:List[Node] = List(),
                         var depth:Int = 0){
    def rPrint(): Unit = {this.children.foreach{e => println("["+this.data.name+"]: "+e); e.rPrint()}}
    override def toString = this.data.name
    def rGet(dat:String): Node = {
      children.foreach{
        e =>  if(e.data==dat) return e
              else return e.rGet(dat)
      }
      null
    }

    def inheritsFrom(className:T):Unit = {
      val newNode = if(nodeView.contains(className.name)){
        val ret = nodeView(className.name)
        if(this.depth > ret.depth)  ret.depth = this.depth-1
        ret
      }else {
        val ret = Node(className, depth = this.depth - 1)
        nodeView += className.name -> ret
        ret
      }
      if(!parents.contains(newNode)) parents = parents.::(newNode)
      if(!newNode.children.contains(this)) newNode.children = newNode.children.::(this)
    }
    def inheritedBy(className:T) = {
      val newNode = if(nodeView.contains(className.name)){
        val ret = nodeView(className.name)
        if(this.depth > ret.depth)  ret.depth = this.depth+1
        ret
      }else{
        val ret = Node(className, depth = this.depth+1)
        nodeView += className.name -> ret
        ret
      }
      if(!children.contains(newNode)) children = children.::(newNode)
      if(!newNode.parents.contains(this)) newNode.parents = newNode.parents.::(this)
    }
  }
}
