package generators.shape

import generators.style.StyleGenerator
import model.shapecontainer.connection.shapeconnections._
import model.shapecontainer.connection.{Connection, Placing}
import model.shapecontainer.shape.Shape
import model.shapecontainer.shape.geometrics._

import scala.collection.mutable

/**
 * Created by julian on 19.01.16.
 * the connection generator
 */
object GeneratorConnectionDefinition {
  val placingsCache =  mutable.HashMap[String, mutable.MutableList[Placing]]()
  val labelCache =  mutable.HashMap[String, mutable.MutableList[Placing]]()

  def generate(connections: Iterable[Connection]) = {
    head +
    """function getConnectionStyle(stylename){
      var style;
      switch(stylename){
    """ +
      connections.map(c => "case " + c.name + ":\n" + {
      if(c.style.isDefined)
        "style = getStyle("+c.style.get.name+");\n" //TODO && connection.style.dslStyle != null ?????? wurde in der style.xtext grammatik nie erwähnt
      else
        "style = {'.connection':{stroke: 'black'}};"
      } +
      generateInlineStyle(c) +
      handlePlacings(c) +
      "break;\n").mkString +
      """default:"
          style = {};
          break;
      }

      return style;
    }

    function getPlacings(stylename){
      var placings;
      switch(stylename){
        """ + generateCachedPlacings + raw"""
        default:
          placings = [];
        break;
      }

      return placings;
    }

    function getLabels(stylename){
      var labels;
      switch(stylename){
        """ + generateCachedLabels + raw"""
        default:
          labels = [];
        break;
      }

      return labels;
    }
    """
  }

  protected def head()={
    raw"""
    /*
     * This is a generated ShapeFile for JointJS
     */
    """
  }

  protected def generateInlineStyle(connection:Connection)={
    if(connection.style isDefined)
    """if(connection.placing)
    //Get inline style
    var inline = {
      '.connection, .marker-target, .marker-source':{
        """ + StyleGenerator.commonAttributes(connection.style.get) + """
        """ + StyleGenerator.fontAttributes(connection.style.get) + """
      }
    };

    //Merge with default style
    jQuery.extend(style, inline);
    """
    else ""
  }

  protected def handlePlacings(connection:Connection )={
    val placings = connection.placing
    var isTargetMarkerSet = false; //Check, whether a target marker is set, because JointJS will show an arrow if none is set
    placings.map(p =>
      {if(p.position_offset == 0.0){
        """style['.marker-source'] = {
        """ + generateMarkerSourceCorrection() + """
                                                 """ + generateMarker(p) + """
          };
       """
      }else if(p.position_offset == 1.0){
        """style['.marker-target'] = {
        """ + generateMarker(p) + """
            };
        """ + {isTargetMarkerSet = true;""}
      }else {
        ""+cachePlacing(connection.name, p)
      }} + "\n" ).mkString +
    {if(!isTargetMarkerSet){
      """style['.marker-target'] = {
            d: 'M 0 0' //override JointJS default arrow
         };
      """
    }}
  }

  protected def generateCachedPlacings()={
    val placings =
    placingsCache.map{case (k, v) => "case \""+k+"""":
      placings = [
      """ + v.map(p => generatePlacing(p) + {if(p != v.last)"," else ""}) + """
    }
    ];
    break;
    """
    }
    placingsCache.clear()
    placings
  }

  protected def generateCachedLabels()= {
    val labels = labelCache.map { case (k, v) =>
      "case \"" + k +
        """":
         labels = [
      """ + v.map(generateLabel).mkString +
        """
    ];
    break;
    """
    labelCache.clear()
    labels
  }}

  protected def generateLabel(placing:Placing)={
    raw"""
    {
      position: """+placing.position_offset +raw""",
      attrs: {
        rect: {fill: 'transparent'},
        text: {dy: """ + placing.position_distance + """}
      },
      id: '""" + placing.shapeCon.asInstanceOf[CDText].textBody+raw"""'
    }
    """
  }

  protected def generatePlacing(placing:Placing )={
    """
    {
      position: """ + placing.position_offset + """,
      """ + generateRightPlacingShape(placing.shapeCon, placing.position_distance.getOrElse(1)) + raw"""
    }"""
  }


  private def generateRightPlacingShape(g:GeometricModel, distance:Int):String = g match{
    case l:Line => generatePlacingShape(l.asInstanceOf[CDLine], distance)
    case pl:PolyLine => generatePlacingShape(pl.asInstanceOf[CDPolyLine], distance)
    case r:Rectangle => generatePlacingShape(r.asInstanceOf[CDRectangle], distance)
    case rr:RoundedRectangle => generatePlacingShape(rr.asInstanceOf[CDRoundedRectangle], distance)
    case p:Polygon => generatePlacingShape(p.asInstanceOf[CDPolygon], distance)
    case e:Ellipse => generatePlacingShape(e.asInstanceOf[CDEllipse], distance)
    case t:Text => generatePlacingShape(t.asInstanceOf[CDText], distance)
  }

  protected def generatePlacingShape(shape:CDLine ,distance:Int)={
    """
    markup: '<line />',
    attrs:{
      x1: """+ shape.points._1.x + """,
      y1: """+ shape.points._1.y + """,
      x2: """+ shape.points._2.x + """,
      y2: """+ shape.points._2.y + """
    }
    """
  }

  protected def generatePlacingShape(shape:CDPolyLine ,distance:Int)={
    """
    markup: '<polyline />',
    attrs:{
      """ + generateStyleCorrections(shape) + """
      points: """" + shape.points.map(point => point.x + ", " + point.y + {if(point != shape.points.last)", " else ""}) + raw"""
    }
    """
  }

  protected def generatePlacingShape(shape:CDRectangle, distance:Int)={
    """
    markup: '<rect />',
    attrs:{
      height: """ + shape.size_height + """,
      width: """ + shape.size_width + """,
      y: """ + (distance - shape.size_height/2) + raw"""
    }
    """
  }

  protected def generatePlacingShape(shape:CDRoundedRectangle ,distance:Int)={
    """
    markup: '<rect />',
    attrs:{
      height: """+shape.size_height + """,
      width: """+shape.size_width + """,
      rx: """ + shape.curve_width + """,
      ry: """ + shape.curve_height + """,
      y: """ + (distance - shape.size_height/2) + raw"""
    }
    """
  }

  protected def generatePlacingShape(shape:CDPolygon , distance:Int)={
    """
    markup: '<polygon />',
    attrs:{
      points: """" + shape.points.map(point => point.x +", "+ point.y + {if(point != shape.points.last)" " else ""}).mkString + raw"""
    }
    """
  }

  protected def generatePlacingShape(shape:CDEllipse , distance:Int)={
    """
    markup: '<ellipse />',
    attrs:{
      rx: """ + shape.size_width/2 + """,
      ry: """ + shape.size_height/2 + """,
      cy: """ + distance + raw"""
    }
    """
  }

  protected def generatePlacingShape(shape:CDText, distance:Int)={
    """
    markup: '<text>«shape.body.value»</text>',
    attrs:{
      y: """ + shape.size_height/2 + """
    }
    """
  }


  protected def generateMarker(placing:Placing )={
    """
    """ + generateRightStyleCorrection(placing.shapeCon) + """
    d: '""" + generateRightSvgPathData(placing.shapeCon) + """'
    """
  }

  private def generateRightSvgPathData(g:GeometricModel):String = g match {
    case l:Line => generateSvgPathData(l.asInstanceOf[CDLine])
    case p:PolyLine => generateSvgPathData(p.asInstanceOf[CDPolyLine])
    case r:Rectangle => generateSvgPathData(r.asInstanceOf[CDRectangle])
    case rr:RoundedRectangle => generateSvgPathData(rr.asInstanceOf[CDRoundedRectangle])
    case p:Polygon => generateSvgPathData(p.asInstanceOf[CDPolygon])
    case e:Ellipse => generateSvgPathData(e.asInstanceOf[CDEllipse])
    case t:Text => generateSvgPathData(t.asInstanceOf[CDText])
  }

  protected def generateSvgPathData(shape:CDLine)={
    val points = shape.points
    """M """ + points._1.x + " " + points._1.y + " L " + points._2.x + " " + points._2.y
  }

  protected def generateSvgPathData(shape:CDPolyLine)={
    val head = shape.points.head
    val tail = shape.points.tail
    """M """ + head.x+" "+head.y + " " + tail.map(point => "L "+point.x+" "+point.y)
  }

  protected def generateSvgPathData(shape:CDRectangle )={
    """M """+shape.x +" " +shape.y + "l " + shape.size_width + " 0 l 0 "+shape.size_height +" l -"+shape.size_width+" 0 z"
  }

  protected def generateSvgPathData(shape:CDRoundedRectangle )={
    "M "+shape.x +" "+ shape.curve_width +" "+shape.y +" "+ shape.curve_height +" l " + (shape.size_width - 2*shape.curve_width) + "l 0 a " + shape.curve_width +" "+ shape.curve_height +" 0 0 1 " +shape.curve_width + " "+shape.curve_height+"l 0 " + (shape.size_height - 2*shape.curve_height)+ " a "+shape.curve_width+" "+shape.curve_height+" 0 0 1 -" +shape.curve_width+" "+shape.curve_height+" l -"+(shape.size_width - 2*shape.curve_width) +" 0 a "+shape.curve_width+" "+shape.curve_height+" 0 0 1 -"+shape.curve_width+" -"+shape.curve_height+" l 0 -"+(shape.size_height - 2*shape.curve_height)+" a "+shape.curve_width+" "+shape.curve_height+" 0 0 1 "+shape.curve_width+" -"+shape.curve_height
  }

  protected def generateSvgPathData(shape:CDPolygon )={
    val head = shape.points.head
    val tail = shape.points.tail
    "M "+head.x+" "+head.y+" "+ tail.map(p => "L "+p.x +" "+p.y).mkString + "z"
  }

  protected def generateSvgPathData(shape:CDEllipse )={
    val rx = shape.size_width / 2
    val ry = shape.size_height / 2
   "M "+shape.x+" "+shape.y+" a  " + rx+" "+ry+" 0 0 1 "+rx+" -"+ry+" a  "+rx+" "+ry+" 0 0 1 "+rx+" "+ry+" a  "+rx+" "+ry+" 0 0 1 -"+rx+" "+ry+" a  "+rx+" "+ry+" 0 0 1 -"+rx+" -"+ry
  }

  protected def generateSvgPathData(shape:CDText )={
    """"""
  }


  private def generateRightStyleCorrection(g:Any):String = g match{
    case pl:PolyLine => generateStyleCorrections(pl.asInstanceOf[CDPolyLine])
    case s:ShapeConnection => generateStyleCorrections(s)
    case _ => ""
  }

  protected def generateStyleCorrections/*(shape:CDPolyLine )*/={
    """
    fill: 'transparent', //JointJS uses fill attribute to fill in all markers
    """
  }

  protected def generateStyleCorrections(shape:ShapeConnection )={
    """"""
  }

  protected def generateMarkerSourceCorrection()={
    """transform: 'scale(1,1)',"""
  }

  protected def cachePlacing(connection:String , placing:Placing ){
    if(placing.shapeCon.isInstanceOf[Text]){
      writeToCache(connection, placing, labelCache)
    }else{
      writeToCache(connection, placing, placingsCache)
    }

  }

  protected def writeToCache(connection:String, placing:Placing , cache: mutable.HashMap[String, mutable.MutableList[Placing]]) {
    if(cache.contains(connection)){
      cache(connection) += placing
    }else{
      val list = mutable.MutableList[Placing]()
        list += placing
      cache.put(connection, list)
    }
  }

  protected def getInlineStyle(shape:Shape )={
    val style = shape.style
    if(style isDefined){"""
     """ + StyleGenerator.commonAttributes(style.get) + """
     """ + StyleGenerator.fontAttributes(style.get) + """
     """
    }
    //TODO unclear... «IF layout.eContainer.eClass == TextLayout»
  }
}
