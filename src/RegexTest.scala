
import model.Cache
import util.SprayParser

object RegexTest extends App {
  val hierarchyContainer = Cache()
  val parser = new SprayParser(hierarchyContainer)


  println("Style\n\n")

  parser.parseRawStyle("""style BpmnDefaultStyle {
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
                 }""")

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
      line-color = blue
      font-size = 5
      }""")

  parser.parseRawStyle(
    """style B {
      line-color = green
      font-size = 7
      }""")

  val testStyle = parser.parseRawStyle(
    """style C extends A, B {
      font-size = 10
      }""")
  println("NOTICE line-color will be green(from style B) and font-size will be 10 -> latest Bound proven")
  println("testStyle.font_size: " + testStyle.head.font_size)
  println("testStyle.line-color: " + testStyle.head.line_color)

  println("\n\nShape")
  parser.parseRawShape("""shape EClassShape style B{
                            size-min (width=4, height=6)
                            size-max (width=10, height=11)
                            stretching (horizontal=true, vertical=false)
                            proportional = true
                            text{
                              size(width=10, height=40)
                              id = Hallo
                              textBody = "pipapo popapi pappipo"
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
                        }""")

  parser.parseRawShape("""//Messages
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
                       }""")

  parser.parseRawConnection("""connection BPMN_DataAssoziation style aicaramba{
                            placing {
                                position (offset=1.0, distance = 1)
                                polygon {
                                    point (x=-10, y=10)
                                    point (x=0, y=0)
                                    point (x=-10, y=-10)
                                    style (font-color = white )
                                }
                            }
                        }""")

  parser.parseRawShape("""
      shape A style aicaramba{
        size-min (width=4, height=6)
        polygon style B{
            point (x=0, y=0)
            point (x=15, y=10)
            point (x=30, y=0)
            style { font-color = green }
        }
      }
    """)

  parser.parseRawShape("""shape B extends A style B{
        size-min (10, 10)
        stretching (true, false)
      }""")

  val C = parser.parseRawShape("""shape C extends B style A{
           text{
             size(width=10, height=40)
             id = Hallo1
             textBody = "hallo Julian"
             style {
              font-color = green
              font-size = 7
             }
           }
           ellipse {
             compartment{
              id = C_ellipse_compartment
              layout = fixed
             }
             size (width=50, height=50)
           }
      }""")

  println("Diagram")
  parser.parseRawDiagram("""diagram DIAGRAM_A for FOO {
       actionGroup actGrp1 {
          action act1 ( label : foo.foo.foo , method : fooImpl1, class : Foo)
          action act2 ( label : foo.foo.foo , method : fooImpl2, class : Foo)
          action act4 ( label : foo.foo.foo , method : fooImpl4, class : Foo)
       }
       edge fooEdge for mock {
        connection : BPMN_DataAssoziation
        from : fromMock
        to : toMock
        palette : fooPal;
        container : fooCont;
          onCreate{
            call action act1
            call actionGroup actGrp1
            askFor : MockReference
          }
          onUpdate{
            call action act1
            call action act2
            call actionGroup actGrp1
          }
          onDelete{
            call action act4
          }
          actions {
            include actGrp1;
            action act3 ( label : foo.foo.foo , method : fooImpl3, class : Foo)
          }
       }
       node fooNode for mock {
          shape : C ( var test -> Hallo1, nest testCompartmentReference -> C_ellipse_compartment, val testText -> Hallo1)
          palette : fooPalette;
          container : fooContainer;
          onCreate{
            call action act1
            call actionGroup actGrp1
            askFor : MockReference
          }
          onUpdate{
            call action act1
            call action act2
            call actionGroup actGrp1
          }
          onDelete{
            call action act4
          }

          actions {
            include actGrp1;
            action act3 ( label : foo.foo.foo , method : fooImpl3, class : Foo)
          }
       }
      }
    """)
}
