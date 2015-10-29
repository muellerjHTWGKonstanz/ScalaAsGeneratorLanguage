
import model.shapecontainer.shape.Shape
import model.style.Style
import model.{ClassHierarchy, Diagram}
import util.SprayParser

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

  val shap = """shape BPMN_EventEskalation_Dash {
                   ellipse {
                       size (width=50, height=50)
                       style (line-style=dash)
                       polygon {
                           point (x=25, y=10)
                           point (x=40, y=40)
                           point (x=25, y=25)
                           point (x=10, y=40)
                       }
                   }
               }"""

  val shap1 = """shape BPMN_EventTimer_default {
                    ellipse {
                        size (width=50, height=50)
                        style (description="hallo")
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

  val shapes = parser.parseRawGeometricModel(shap)
  println(shapes.head)//TODO why cant i access shapes(0).wraps?!?!?!
}
