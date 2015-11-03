
import model.shapecontainer.shape.Shape
import model.style.{StyleParser, Style}
import model.{ClassHierarchy, Diagram}
import util.SprayParser

object RegexTest extends App {
  val diagram = Diagram(new ClassHierarchy[Style](new Style(name = "root")), new ClassHierarchy[Shape](new Shape(name = "root")))
  val parser = new SprayParser(diagram)
  val styleUno = """style BpmnDefaultStyle {
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
                  gradient-area-offset = 10
                 }"""
  parser.parseRawStyle(styleUno)


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

  val shapeWithText = """shape EClassShape {
                            size-min (width=4, height=6)
                            size-max (width=10, height=11)
                            stretching (horizontal=true, vertical=false)
                            proportional = true
                            anchor {
                              position (x=10, y=20)
                              position (x=12, y=90)
                            }
                            description style aicaramba{
                              align (horizontal=center, verrtical=top)
                              id = BABABU
                            }
                            text{
                              size(width=10, height=40)
                              id = Hallo
                            }
                            rectangle {
                              style (line-width=2)
                              position (x=2, y=0)
                              size (width=10, height=3)
                              ellipse {
                                  position (x=0, y=36)
                                  size (width=30, height=30)
                            	}
                            }
                        }"""
  val shapesList = parser.parseRawShape(shapeWithText)
  println(shapesList)
}
