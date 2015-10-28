
import model.shapecontainer.shape.Shape
import model.style.Style
import model.{ClassHierarchy, Diagram}
import util.SprayParser

import scala.util.parsing.combinator.JavaTokenParsers

object RegexTest extends App {
  val classUno = """style BpmnDefaultStyle {
                  description = "The default style of the petrinet diagram type."
                  transparency = 0.95
                  background-color = green
                  line-color = black
                  line-style = solid
                  line-width = 1
                  font-color = white
                  font-name = "Tahoma"
                  font-size = 6
                  font-bold = yes
                  font-italic = yes
                  gradient-orientation = horizontal
                 }"""

  val shap = """shape BPMN_EventEnd_used {
                   ellipse {
                       size (width=50, height=50)
                       ellipse {
                           foo = bar
                       }
                   }
                   ellipse {
                       size (width=50, height=50)
                       ellipse {
                           foo = bar
                       }
                   }
               }"""

  val shap1 = """shape BPMN_EventTimer_default {
                    ellipse style BpmnDefaultStyle{
                        size (width=50, height=50)
                    }
                    ellipse {
                        size (width=50, height=50)
                        style (line-width=1)

                        //Clock
                        ellipse {
                            size (width=38, height=38)
                            position (x=6, y=6)
                            polyline {
                                point (x=32, y=19)
                                point (x=19, y=19)
                                point (x=22, y=3)
                            }
                            line {
                                point (x=19, y=0)
                                point (x=19, y=4)
                            }
                        }
                    }
                }"""

  val diagram = Diagram(new ClassHierarchy[Style](new Style(name = "root")), new ClassHierarchy[Shape](new Shape(name = "root")))
  val parser = new SprayParser(diagram)

  parser.parseRawStyle(classUno)
  val shapeSketches = parser.parseRawShape(shap1)
  val allShapes= for(i<-shapeSketches)yield{i.parse(None)}

  parser.parseRawStyle(
    """style BPMNDefault {
      line-color = 40
      font-size = 10
      }""")

  parser.parseRawStyle(
    """style aicaramba extends BPMNDefault {
      line-color = blue
      font-italic = yes
      }""")

  println(allShapes.mkString)

  //println(parse(attribute, "x-y=12"))
  //println(parseAttributes("style (l-vb=12, lone=11)"))
}
