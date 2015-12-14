
import model.Cashe
import util.SprayParser

object RegexTest extends App {
  val hierarchyContainer = Cashe()
  val parser = new SprayParser(hierarchyContainer)
  val styleUno = """style BpmnDefaultStyle {
                  description = "The default style of the petrinet hierarchyContainer type."
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
    """style aicaramba {
      line-color = blue
      font-italic = false
      }""")

  parser.parseRawStyle(
    """style BPMNDefault {
      line-color = green
      font-size = 10
      }""")

  parser.parseRawStyle(
    """style A {
      line-color = green
      font-size = 10
      }""")

  parser.parseRawStyle(
    """style B {
      line-color = green
      font-size = 10
      }""")

  parser.parseRawStyle(
    """style C extends A, B {
      line-color = green
      font-size = 10
      }""")
  val shapeWithText = """shape EClassShape style B{
                            size-min (width=4, height=6)
                            size-max (width=10, height=11)
                            stretching (horizontal=true, vertical=false)
                            proportional = true
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
                            description style A{
                              align (horizontal=center, vertical=top)
                              id = BABABA
                            }
                            anchor = center
                        }"""

  val nonfailingShape = """//Messages
                       shape BPMN_EventMail  style BpmnDefaultStyle{
                           ellipse style aicaramba{
                               compartment(
                                  id = blablablu
                                  layout = fixed
                                  stretching (horizontal = true, vertical = false)
                                  spacing = 12
                                  margin = 10
                                  invisible = invisible
                                )
                               size (width=50, height=50)
                               rectangle {
                                   position (x=10, y=15)
                                   size (width=30, height=20)
                                   style (line-width=2)
                                   compartment(
                                        id = fooID
                                        layout = fixed
                                   )
                                   polygon {
                                       point (x=0, y=0)
                                       point (x=15, y=10)
                                       point (x=30, y=0)
                                   }
                               }
                           }
                       }"""
  parser.parseRawShape(shapeWithText)
  val shapesList = parser.parseRawShape(nonfailingShape)
  println(shapesList)


  val connectionUno = """connection BPMN_DataAssoziation style aicaramba{
                            placing {
                                position (offset=1.0, distance = 1)
                                polygon {
                                    point (x=-10, y=10)
                                    point (x=0, y=0)
                                    point (x=-10, y=-10)
                                    style (background-color=black)
                                }
                            }
                        }"""

  val conni = parser.parseRawConnection(connectionUno)
  println(conni)


  val shapeA =
    """
      shape A style aicaramba{
        size-min (width=4, height=6)
        polygon style B{
            point (x=0, y=0)
            point (x=15, y=10)
            point (x=30, y=0)
        }
      }
    """

  val shapeB =
    """shape B extends A style B{
        stretching (horizontal=true, vertical=false)
      }"""
  val shapeC =
    """shape C extends B style A{
           text{
             size(width=10, height=40)
             id = Hallo
           }
      }"""

  parser.parseRawShape(shapeA)
  parser.parseRawShape(shapeB)
  val testShapes = parser.parseRawShape(nonfailingShape)
  println(testShapes)
}
