import generators.StyleGenerator
import model.Cache
import util.SprayParser

/**
 * Created by julian on 9/3/15.
 * Diverse Tests für die Klassen StringToObjectParser ClassHierarchy Diagram Style
 */
object ParserApp extends App {

  val hierarchyContainer = Cache()

  val classUno = """style BpmnDefaultStyle {
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
                 }"""

  val classDuo = """style BpmnExtending extends BpmnDefaultStyle{
                  line-color = green
                }"""

  val parser = new SprayParser(hierarchyContainer)

  //val newStyle = StringToObjectParser toStyle(classUno, hierarchyContainer)
  //val anotherStyle = StringToObjectParser toStyle(classDuo, hierarchyContainer)
  val newStyle = parser.parseRawStyle(classUno)
  val anotherStyle = parser.parseRawStyle(classDuo)

  hierarchyContainer.styleHierarchy.root.rPrint()
  //println(hierarchyContainer.styleHierarchy(newStyle).data.lineColor)
  //println(hierarchyContainer.styleHierarchy(anotherStyle).data.lineColor)

  val classTres =
    """style yetAnotherStyle extends BpmnExtending{
      font-size = 10
      }"""
  //val yetAnotherStyle = StringToObjectParser toStyle(classTres, hierarchyContainer)
  val yetAnotherStyle = parser.parseRawStyle(classTres)
  //println(hierarchyContainer.styleHierarchy(newStyle).data.       fontSize)
  //println(hierarchyContainer.styleHierarchy(anotherStyle).data.   fontSize)
  //println(hierarchyContainer.styleHierarchy(yetAnotherStyle).data.fontSize)

  hierarchyContainer.styleHierarchy.root.rPrint()


  var differentStyle =
    """style A{
      font-size = 20
      }""".stripMargin
  val A = parser.parseRawStyle(differentStyle)

  differentStyle =
  """style B extends A{
    line-color = blue
    }"""
  val B = parser.parseRawStyle(differentStyle)

  differentStyle =
    """style C extends B{
      description = "The default style of the petrinet hierarchyContainer type."
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
  val C = parser.parseRawStyle(differentStyle)

  //println(hierarchyContainer.styleHierarchy(A).data.line_color)
  //println(hierarchyContainer.styleHierarchy(B).data.line_color)
  //println(hierarchyContainer.styleHierarchy(C).data.line_color)

  //println(hierarchyContainer.styleHierarchy(A).data.font_size)
  //println(hierarchyContainer.styleHierarchy(B).data.font_size)
  //println(hierarchyContainer.styleHierarchy(C).data.font_size)

  println(StyleGenerator.compileDia(A))
}

