package util

/**
 * Created by julian on 24.09.15.
 */

sealed class ClassHierarchyOrderedByString{

  val root = Node("root")
  var nodeView:Map[String, Node] = Map("root" -> root)

  def apply(parent:Node, className:String) = parent.isInheritedBy(className)
  def apply(parent:String, className:String) = nodeView(parent).isInheritedBy(className)
  def apply(className:String)=nodeView(className)
  override def toString() = root.toString()

  def newBaseClass(className:String) = root.isInheritedBy(className)
  def get(styleName:String) = nodeView(styleName)
  def setRelation(parent:String, child:String) = nodeView(parent) isInheritedBy child
  def setRelation(parent:Node, child:String) = parent isInheritedBy child

  sealed case class Node(className: String,
                         var parents:List[Node] = List(),
                         var children:List[Node] = List(),
                         var depth:Int = 0){

    def rPrint(): Unit = {this.children.foreach{e => println("["+this.className+"]: "+e); e.rPrint()}}
    override def toString() = this.className + children.foreach(e => e.toString)
    def rGet(dat:String): Node = {
      children.foreach{
        e =>  if(e.className==dat) return e
              else return e.rGet(dat)
      }
      null
    }

    def inheritsFrom(className:String):Unit = {
      val newNode = if(nodeView.contains(className)){
        val ret = nodeView(className)
        if(this.depth > ret.depth)  ret.depth = this.depth-1
        ret
      }else {
        val ret = Node(className, depth = this.depth - 1)
        nodeView += className -> ret
        ret
      }
      if(!parents.contains(newNode)) parents = parents.::(newNode)
      if(!newNode.children.contains(this)) newNode.children = newNode.children.::(this)
    }
    def isInheritedBy(className:String):Unit = {
      val newNode = if(nodeView.contains(className)){
        val ret = nodeView(className)
        if(this.depth > ret.depth)  ret.depth = this.depth+1
        ret
      }else{
        val ret = Node(className, depth = this.depth+1)
        nodeView += className -> ret
        ret
      }
      if(!children.contains(newNode)) children = children.::(newNode)
      if(!newNode.parents.contains(this)) newNode.parents = newNode.parents.::(this)
    }
  }
}
