
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
  println(parser.parseRawStyle(
    """style BPMNDefault {
      line-color = 40
      font-size = 10
      }"""))

  println(parser.parseRawStyle(
    """style aicaramba extends BPMNDefault {
      line-color = blue
      font-italic = yes
      }"""))


  //println(parser.parseRawStyle(classUno))
  //val allShapes= for(i<-parser.parseRawShape(shap1))yield{i.parse(None)}
  //println(allShapes.mkString)

  object Parser extends JavaTokenParsers{
    def attributes = "style (" ~> rep(attribute) <~ ")" ^^ {case attr => attr}
    def attribute:Parser[(String, String)] = variable ~ argument <~ ",?".r ^^ {case v ~ a => (v.toString,a.toString)}
    def argument = "(([a-züäö]+([-_][a-züäö])?)|(\".*\")|([+-]?\\d+(\\.\\d+)?))".r ^^ {_.toString}
    def variable:Parser[String] = ident <~ "="  ^^ {case varname => varname.toString}

    def parseAttributes(input:String) = parse(attributes, input).get
  }
  import Parser._
  println(parse(attributes, "style (x-y=12, y=asd)"))
  println(parseAttributes("style (x=12 y=adj)"))

  //println(parser.parse(parser.argument_classic, " = \"302\""))
  //println(parser.parse(parser.argument_advanced_explicit, "(foo-fo = 12.12, faa-f = \"bla\", fer = gaaaab)"))
  //println(parser.parse(parser.argument_advanced_implicit, "(2, \"hallo\", 24.12, gabruu)"))
}
