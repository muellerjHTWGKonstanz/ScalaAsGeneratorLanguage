package generators.shape

import java.util.UUID
import model.shapecontainer.shape.geometrics.layouts.CommonLayout
import scala.collection.mutable
import model.shapecontainer.shape.Shape
import model.shapecontainer.shape.geometrics._
import model.shapecontainer.shape.geometrics.GeometricModel

/**
 * Created by julian on 19.01.16.
 * The actual ShapeGenerator
 */
object GeneratorShapeDefinition {
  val attrs = mutable.HashMap[String, mutable.MutableList[String]]()
  val attrsInspector = mutable.HashMap[String, mutable.HashMap[GeometricModel, String]]()

  def head(packageName:String) = {
    raw"""
    /*
     * This is a generated ShapeFile for JointJS
     */

    if (typeof exports === 'object') {

      var joint = {
        util: require('../src/core').util,
        shapes: {
          basic: require('./joint.shapes.basic')
        },
        dia: {
          ElementView: require('../src/joint.dia.element').ElementView,
          Link: require('../src/joint.dia.link').Link
        }
      };
    }

    joint.shapes.""" + packageName + """ = {};

    """
  }

  def generate(shape:Shape, packageName:String) = {
    """
    joint.shapes.""" + packageName + "." + shape.name + """ = joint.shapes.basic.Generic.extend({
      markup: """ + generateSvgMarkup(shape) + """,
        defaults: joint.util.deepSupplement({
        type: '""" + packageName+ "."+ shape.name + """',
        size: {width: """+calculateWidth(shape) + ", height: "+calculateHeight(shape)+"""},
        attrs: {
          'rect.bounding-box':{
          height: """+calculateHeight(shape)+""",
          width: """+calculateWidth(shape)+"""
        },
          """+generateAttrs(shape.name)+raw"""
        }
      }, joint.dia.Element.prototype.defaults)
    });

    """
  }

  protected def generateSvgMarkup(shape:Shape ) = {
    """ '<g class="rotatable"><g class="scalable"><rect class="bounding-box" />"""+ {for(s <- shape.shapes.getOrElse(List())) yield {generateSvgShape(s, shape.name, "scalable") + "</g></g>'"}}
  }

  private def generateSvgShape(g:GeometricModel, shapeName:String, parentClass:String):String = {
    g match {
      case g:Line => generateSvgShape(g.asInstanceOf[Line], shapeName, parentClass)
      case g:Rectangle => generateSvgShape(g.asInstanceOf[Rectangle], shapeName, parentClass)
      case g:Ellipse => generateSvgShape(g.asInstanceOf[Ellipse], shapeName, parentClass)
      case g:Polygon => generateSvgShape(g.asInstanceOf[Polygon], shapeName, parentClass)
      case g:PolyLine => generateSvgShape(g.asInstanceOf[PolyLine], shapeName, parentClass)
      case g:RoundedRectangle => generateSvgShape(g.asInstanceOf[RoundedRectangle], shapeName, parentClass)
      case g:Text => generateSvgShape(g.asInstanceOf[Text], shapeName, parentClass)
    }
  }

  protected def generateSvgShape(shape:Line,rootShapeName:String ,parentClass:String):String = {
    val className = UUID.randomUUID
    buildAttrs(shape, rootShapeName, className.toString, parentClass)
    s"""<line class="$className" />"""
  }

  protected def generateSvgShape(shape:Rectangle, rootShapeName:String, parentClass:String):String={
    val className = UUID.randomUUID
    buildAttrs(shape, rootShapeName, className.toString, parentClass)
    s"""<rect class="$className" />""" + shape.children.map{subShape => generateSvgShape(subShape, rootShapeName, className.toString)}.mkString
  }

  protected def generateSvgShape(shape:Ellipse, rootShapeName:String, parentClass:String):String={
    val className = UUID.randomUUID
    buildAttrs(shape, rootShapeName, className.toString, parentClass)
    s"""<ellipse class="$className" />""" + shape.children.map{subShape => generateSvgShape(subShape, rootShapeName, className.toString)}.mkString
  }

  protected def generateSvgShape(shape:Polygon, rootShapeName:String, parentClass:String):String={
    val className = UUID.randomUUID
    buildAttrs(shape, rootShapeName, className.toString, parentClass)
    s"""<polygon class="$className" />""" + shape.children.map{subShape => generateSvgShape(subShape, rootShapeName, className.toString)}.mkString
  }

  protected def generateSvgShape(shape:PolyLine, rootShapeName:String, parentClass:String):String={
    val className = UUID.randomUUID
    buildAttrs(shape, rootShapeName, className.toString, parentClass)
    s"""<polyline class="$className" />"""
  }

  protected def generateSvgShape(shape:RoundedRectangle, rootShapeName:String, parentClass:String):String={
    val className = UUID.randomUUID
    buildAttrs(shape, rootShapeName, className.toString, parentClass)
    s"""<rect class="$className" />""" + shape.children.map{subShape => generateSvgShape(subShape, rootShapeName, className.toString)}.mkString
  }

  protected def generateSvgShape(shape:Text, rootShapeName:String, parentClass:String):String={
    val className = UUID.randomUUID
    val shapeValue = shape.textBody
    buildAttrs(shape, rootShapeName, className.toString, parentClass)
    s"""<text class="$className $shapeValue" />"""
  }



  protected def generateAttrs(shapeName:String):String = {
    val classes = attrs.get(shapeName)
    if(classes isEmpty) return ""
    val text = classes.get.map{c => c + {if(c != classes.get.last)"," else ""}}.mkString
    attrs.clear()
    text
  }

  protected def buildAttrs(shape:GeometricModel, shapeName: String, className:String, parentClass:String)={
    val attributes = s"""
    '.$className':{
      """+ getAttributes(shape, parentClass) + "\n}"

    if(attrs.contains(shapeName)){
      attrs(shapeName) += attributes
      attrsInspector(shapeName).put(shape,className)
    }else{
      val list =  mutable.MutableList[String]()
      list += attributes

      val att = new mutable.HashMap[GeometricModel, String]
        att.put(shape, className)
      attrs.put(shapeName, list)
      attrsInspector.put(shapeName, att)
    }

  }

  private def getAttributes(shape:GeometricModel, parentClass:String):String = {
    shape match {
      case l:Line => getAttributes(l.asInstanceOf[Line], parentClass)
      case l:Rectangle => getAttributes(l.asInstanceOf[Rectangle], parentClass)
      case l:Ellipse => getAttributes(l.asInstanceOf[Ellipse], parentClass)
      case l:RoundedRectangle => getAttributes(l.asInstanceOf[RoundedRectangle], parentClass)
      case l:PolyLine => getAttributes(l.asInstanceOf[PolyLine], parentClass)
      case l:Polygon => getAttributes(l.asInstanceOf[Polygon], parentClass)
      case l:Text => getAttributes(l.asInstanceOf[Text], parentClass)
    }
  }

  protected def getAttributes(shape:Line, parentClass:String):String={
    val shapex1 = shape.x1
    val shapey1 = shape.y1
    val shapex2 = shape.x2
    val shapey2 = shape.y2
    s"""
    x1: $shapex1,
    y1: $shapey1,
    x2: $shapex2,
    y2: $shapey2
    """
  }

  protected def getAttributes(shape:Rectangle, parentClass: String) = {
    generatePosition(shape) +
    "'width': " +  shape.size_width + ",\n"
    "'height': "+ shape.size_height + "\n"
  }

  protected def getAttributes(shape:Ellipse,  parentClass:String) = {
    generatePosition(shape) +
    "rx: " + shape.size_width / 2 +",\n"
    "ry: " + shape.size_height / 2 + "\n"
  }

  protected def getAttributes(shape:Polygon,  parentClass:String) = {
    """
    """
  }

  protected def getAttributes(shape:PolyLine, parentClass:String) = {
    """
    points: """"+ shape.points.map{ p => getX(p, shape) + ", " + getY(p, shape)}.mkString + """
    """
  }

  protected def getAttributes(shape:RoundedRectangle , parentClass:String)={
    "\n"+generatePosition(shape) +
    "'width': " + shape.size_width + ",\n" +
    "'height': " +shape.size_height + ",\n" +
    "rx: " + shape.curve_width + ",\n"+
    "ry: " + shape.curve_height +"\n"
  }

  protected def getAttributes(shape:Text , parentClass:String)={
    "\n"+generatePosition(shape) +
    "'width': " + shape.size_width + ",\n" +
    "'height': " + shape.size_height + ",\n" +
    """text: """" + shape.textBody + """" //Is overwritten in stencil, but needed here for scaling
    """
  }

  private def generatePosition(shape:GeometricModel):String = {
    shape match {
      case r:Rectangle => generatePosition(shape.asInstanceOf[Rectangle])
      case r:Ellipse => generatePosition(shape.asInstanceOf[Ellipse])
      case r:RoundedRectangle => generatePosition(shape.asInstanceOf[RoundedRectangle])
      case r:Text => generatePosition(shape.asInstanceOf[Text])
    }
  }

  protected def generatePosition(shape:Rectangle)={
    val (x, y) = (shape.position.get._1, shape.position.get._2)
    s"""k
    x: $x,
    y: $y,
    """
  }

  protected def generatePosition(shape:Ellipse)={
    val (x, y) = (shape.position.get._1, shape.position.get._2)
    s"""k
    cx: $x,
    cy: $y,
    """
  }

  protected def generatePosition( shape:RoundedRectangle)={
    val (x, y) = (shape.position.get._1, shape.position.get._2)
    s"""k
    x: $x,
    y: $y,
    """
  }

  protected def generatePosition(shape:Text )={
    val (x, y) = (shape.position.get._1, shape.position.get._2)
    s"""k
    x: $x,
    y: $y,
    """
  }

  protected def calculateWidth(shapeDef:Shape) =
    shapeDef.shapes.getOrElse(List()).foldLeft(0)((acc, g) => g match {
      case layout: CommonLayout if layout.size_width > acc =>  layout.size_width
      case _ => acc
    })

  protected def getWidth(shape:Line )={maxX(List(shape.points._1, shape.points._2))}
  protected def getWidth(shape:Rectangle )={shape.x + shape.size_width}
  //Ellipse is automatically positioned with xcor + radius. Thus, we need xcor + radius + radius = xcor + diameter here.
  protected def getWidth(shape:Ellipse )={shape.x + shape.size_width}
  protected def getWidth(shape:Polygon )={maxX(shape.points)}
  protected def getWidth(shape:PolyLine )={maxX(shape.points)}
  protected def getWidth(shape:RoundedRectangle)={shape.x + shape.size_width}
  protected def getWidth(shape:Text )={shape.x + shape.size_width}

  protected def maxX(points:List[Point])={
    var max = points.head.x
    for(p <- points){
      if(p.x > max){
        max = p.x
      }
    }
    max
  }

  protected def minX(points:List[Point])={
    var min = points.head.x
    for(p <- points){
      if(p.x < min){
        min = p.x
      }
    }
    min
  }

  protected def calculateHeight(shapeDef:Shape)={
    var height = 0
    shapeDef.shapes.getOrElse(List()).foreach {
      case c: CommonLayout if c.size_height > height => height = c.size_height
      case _ =>
    }
    height
  }

  protected def getHeight(shape:Line) = maxY(List(shape.points._1, shape.points._2))
  protected def getHeight(shape:Rectangle ) = shape.y + shape.size_height
  //Ellipse is automatically positioned with ycor + radius. Thus we need ycor + radius + radius = ycor + diameter here
  protected def getHeight(shape:Ellipse) = shape.y + shape.size_height
  protected def getHeight(shape:Polygon) = maxY(shape.points)
  protected def getHeight(shape:PolyLine) = maxY(shape.points)
  protected def getHeight(shape:RoundedRectangle ) = shape.y + shape.size_height
  protected def getHeight(shape:Text) = shape.y + shape.size_height


    protected def maxY(points:List[Point]) = {
      var max = points.head.y
      for(p <- points){
        if(p.y > max){
          max = p.y
        }
      }
      max
    }

  protected def minY(points:List[Point])={
    var min = points.head.y
    for(p <- points){
      if(p.y < min){
        min = p.y
      }
    }
    min
  }

  protected def getX(point:Point, shape:GeometricModel)={
    if(shape.parent isEmpty){
      point.x
    }else{
      point.x + callRightReferenceX(shape.parent.get) //last because multiple inheritance is now possible and latest bound principle
    }
  }

  protected def getY(point:Point, shape:GeometricModel)={
    if(shape.parent isEmpty){
      point.y
    }else{
      point.y + callRightReferenceY(shape.parent.get)
    }
  }

  protected def getX1(shape:Line )={
    if(shape.parent isEmpty){
      shape.points._1.x
    }else{
      shape.points._1.x + callRightReferenceX(shape.parent.get)
    }
  }

  protected def getX2(shape:Line )={
    if(shape.parent isEmpty){
      shape.points._2.x
    }else{
      shape.points._2.x + callRightReferenceX(shape.parent.get)
    }
  }

  protected def getY1(shape:Line)={
    if(shape.parent isEmpty){
      shape.points._1.y
    }else{
      shape.points._2.y + callRightReferenceX(shape.parent.get)
    }
  }

  protected def getY2(shape:Line )={
    if(shape.parent isEmpty){
      shape.points._1.y
    }else{
      shape.points._2.y + callRightReferenceX(shape.parent.get)
    }
  }

  protected def getX(shape:Rectangle)={
    if(shape.parent isEmpty){
      shape.x
    }else{
      shape.x + callRightReferenceX(shape.parent.get)
    }
  }

  protected def getY(shape:Rectangle )={
    if(shape.parent isEmpty){
      shape.y
    }else{
      shape.y + callRightReferenceY(shape.parent.get)
    }
  }

  protected def getX(shape:RoundedRectangle)={
    if(shape.parent isEmpty){
      shape.x
    }else{
      shape.x + callRightReferenceX(shape.parent.get)
    }
  }

  protected def getY(shape:RoundedRectangle)={
    if(shape.parent isEmpty){
      shape.y
    }else{
      shape.y + callRightReferenceY(shape.parent.get)
    }
  }

  protected def getX(shape:Text)={
    if(shape.parent isEmpty){
      shape.x
    }else{
      shape.x + callRightReferenceX(shape.parent.get)
    }
  }

  protected def getY(shape:Text)={
    if(shape.parent isEmpty){
      shape.y
    }else{
      shape.y + callRightReferenceY(shape.parent.get)
    }
  }

  protected def getCx(shape:Ellipse)={
    if(shape.parent isEmpty){
      (shape.size_width /2) + shape.x
    }else{
      (shape.size_width /2) + shape.x + callRightReferenceX(shape.parent.get)
    }
  }

  protected def getCy(shape:Ellipse)={
    if(shape.parent isEmpty){
      (shape.size_height /2) + shape.y
    }else{
      (shape.size_height /2) + shape.y + callRightReferenceY(shape.parent.get)
    }
  }


  private def callRightReferenceX(g:GeometricModel):Int = g match {
    case r:Rectangle => referenceX(r)
    case e:Ellipse => referenceX(e)
    case p:Polygon => referenceX(p)
    case r:RoundedRectangle => referenceX(r)
    case _ => 0
  }

  private def callRightReferenceY(g:GeometricModel):Int = g match {
    case r:Rectangle => referenceY(r)
    case e:Ellipse => referenceY(e)
    case p:Polygon => referenceY(p)
    case r:RoundedRectangle => referenceY(r)
    case _ => 0
  }

  protected def referenceX(shape:Rectangle ):Int = {
    if(shape.parent isEmpty){
      shape.x
    }else{
      shape.x + callRightReferenceX(shape.parent.get)
    }
  }

  protected def referenceX(shape:Ellipse )={
    if(shape.parent isEmpty){
      shape.x
    }else{
      shape.x + callRightReferenceX(shape.parent.get)
    }
  }

  protected def referenceX( shape:Polygon)={
    if(shape.parent isEmpty){
      minX(shape.points)
    }else{
      minX(shape.points) + callRightReferenceX(shape.parent.get)
    }
  }

  protected def referenceX(shape:RoundedRectangle )={
    if(shape.parent isEmpty){
      shape.x
    }else{
      shape.x + callRightReferenceX(shape.parent.get)
    }
  }

  protected def referenceY(shape:Rectangle )={
    if(shape.parent isEmpty){
      shape.y
    }else{
      shape.y + callRightReferenceY(shape.parent.get)
    }
  }

  protected def referenceY(shape:Ellipse )={
    if(shape.parent isEmpty){
      shape.y
    }else{
      shape.y + callRightReferenceY(shape.parent.get)
    }
  }

  protected def referenceY(shape:Polygon )={
    if(shape.parent isEmpty){
      minY(shape.points)
    }else{
      minY(shape.points) + callRightReferenceY(shape.parent.get)
    }
  }

  protected def referenceY(shape:RoundedRectangle )={
    if(shape.parent isEmpty){
      shape.y
    }else{
      shape.y + callRightReferenceY(shape.parent.get)
    }
  }


  protected def getParent(shape:Shape )={
    if(shape.extendedShape isEmpty){
      null
    }else{
      shape.extendedShape last
    }
  }
}
