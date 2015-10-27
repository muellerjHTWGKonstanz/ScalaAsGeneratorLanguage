
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
                    ellipse {
                        size (width=50, height=50)
                        point {
                            size (width=46, height=46)
                            position (x=2, y=2)
                        }
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
  println(parser.parse(parser.style,
    """style BPMNDefault {
      line-color = 40
      font-size = 10
      }"""))

  println(parser.parse(parser.style,
    """style aicaramba extends BPMNDefault {
      line-color = blue
      font-italic = yes
      }"""))

  //println(parser.parse(parser.style, classUno))

  //println(parser.parseRawShape(shap1))
  println(parser.parseRawStyle(classUno))

  //println(parser.parse(parser.argument_classic, " = \"302\""))
  //println(parser.parse(parser.argument_advanced_explicit, "(foo-fo = 12.12, faa-f = \"bla\", fer = gaaaab)"))
  //println(parser.parse(parser.argument_advanced_implicit, "(2, \"hallo\", 24.12, gabruu)"))
}
