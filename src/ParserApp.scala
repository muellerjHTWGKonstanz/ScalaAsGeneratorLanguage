import model.{ClassHierarchy, Style, Diagram}
import util.StringToObjectParser

/**
 * Created by julian on 9/3/15.
 * Diverse Tests für die Klassen StringToObjectParser ClassHierarchy Diagram Style
 */
object ParserApp extends App {

  val diagram = Diagram(new ClassHierarchy[Style](new Style(name = "root")))

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

  val classDuo = """style BpmnExtending extends BpmnDefaultStyle{
                  line-color = green
                }"""


  val newStyle = StringToObjectParser toStyle(classUno, diagram)
  val anotherStyle = StringToObjectParser toStyle(classDuo, diagram)

  diagram.styleHierarchy.root.rPrint()
  //println(diagram.styleHierarchy(newStyle).data.lineColor)
  //println(diagram.styleHierarchy(anotherStyle).data.lineColor)

  val classTres =
    """style yetAnotherStyle extends BpmnExtending{
      font-size = 10
      }"""
  val yetAnotherStyle = StringToObjectParser toStyle(classTres, diagram)
  //println(diagram.styleHierarchy(newStyle).data.       fontSize)
  //println(diagram.styleHierarchy(anotherStyle).data.   fontSize)
  //println(diagram.styleHierarchy(yetAnotherStyle).data.fontSize)

  diagram.styleHierarchy.root.rPrint()


  var differentStyle =
    """style A{
      font-size = 20
      }""".stripMargin
  val A = StringToObjectParser toStyle(differentStyle, diagram)

  differentStyle =
  """style B extends A{
    line-color = blue
    }"""
  val B = StringToObjectParser toStyle(differentStyle, diagram)

  differentStyle =
    """style C extends B{
      description = "The default style of the petrinet diagram type."
      transparency = 0.95
      background-color = green
      line-style = solid
      line-color = white
      line-width = 1
      font-color = white
      font-name = "Tahoma"
      font-bold = yes
      font-italic = yes
      gradient-orientation = horizontal
      }"""
  val C = StringToObjectParser toStyle(differentStyle, diagram)

  println(diagram.styleHierarchy(A).data.lineColor)
  println(diagram.styleHierarchy(B).data.lineColor)
  println(diagram.styleHierarchy(C).data.lineColor)

  println(diagram.styleHierarchy(A).data.fontSize)
  println(diagram.styleHierarchy(B).data.fontSize)
  println(diagram.styleHierarchy(C).data.fontSize)

}

