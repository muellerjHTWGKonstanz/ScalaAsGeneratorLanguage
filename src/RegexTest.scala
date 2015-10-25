
import model.Diagram
import util.ShapeParser

object RegexTest extends App {
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

  //val regex = "(?s)(ellipse|point|line|polyline) \\{[^\\{.+\\}]+\\}".r //gets ll inner shapes
  //val regex1 = "(?s)(?!(ellipse|point|line|polyline) \\{.+)(ellipse|point|line|polyline) \\{.*\\}".r //trying to get only parent shapes
  //val ret = regex1.findAllIn(shap1Modded).toArray
  //println(ret.deep)
  //println(ret.length)

  import util.ShapeParser
  //println(new ShapeParser(new Diagram()).parseRawShape(shap1))
  println("style (Foo = Bar)".matches("style.+"))
  println("parent = Foo".replaceFirst("parent", "").replaceAll("\\(|\\)|\\=", "").trim)
  //println(ShapeParser.parse(ShapeParser.attribute, "size (x-w=2, y_b=2)"))



}
