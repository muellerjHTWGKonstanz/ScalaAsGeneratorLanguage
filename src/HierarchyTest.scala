import model.{Shape, Style, ClassHierarchy, Diagram}
import util.StringToObjectParser

/**
 * Created by julian on 13.10.15.
 * just a little test
 */
object HierarchyTest extends App{

  val diagram = new Diagram(new ClassHierarchy[Style](new Style(name = "root")),
                            new ClassHierarchy[Shape](new Shape(name = "root")))

  val class1 =
    """style A {
      |transparency = 2.0
      |line-width = 5
      |font-size = 100
      |}""".stripMargin

  val class2 =
    """style B {
      |transparency = 4.0
      |line-width = 6
      |}""".stripMargin

  val class3 =/*only A and B are valid Hallo and C wont be in classHierarchy, redundant call to B will add an additional for loop in StringToObjectParser but nothing else*/
    """style C extends A, B, B, Hallo, C{
      |line-width = 7
      |}""".stripMargin

  val class4 =
    """style D extends C{
      |line-width = 7
      |}""".stripMargin

  val A = StringToObjectParser toStyle(class1, diagram)
  val B = StringToObjectParser toStyle(class2, diagram)
  val C = StringToObjectParser toStyle(class3, diagram)
  val D = StringToObjectParser toStyle(class4, diagram)


  /*see, C extends A and B*/
  diagram.styleHierarchy.root.rPrint()

  /*see, C has most relevant line-width = 7(from self), most relevant transparency 4(from most important parent)
   and font-size = 100(from second most important parent)*/
  println("style C: "+ C.line_width.get)
  println("style C: "+ C.transparency.get)
  println("style C: "+ C.font_size.get)
  println("Style D: "+ D.font_size.get + " <- even D has A's font-size, for C inherits from A and D inherits from C")


}
