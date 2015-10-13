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
    "style D extends C"

  val A = StringToObjectParser toStyle(class1, diagram)
  val B = StringToObjectParser toStyle(class2, diagram)
  val C = StringToObjectParser toStyle(class3, diagram)
  val D = StringToObjectParser toStyle(class4, diagram)

  println("created style-classes A, B, C, D like the following")
  println(
    raw"""
            root
          ____|___
          |      |
          A      B
          |______|
          C      |
          |______|
          D
       """)

  diagram.styleHierarchy.root.rPrint()
  println("see, C extends A and B (rprint-methode from classHierarchy shows [parent]:child):")
  println("\n")
  println("A, B, C and D are declared like this:")
  println(class1)
  println(class2)
  println(class3)
  println(class4)

  println("\naccessing the following attributes should show, that inheritance was successful!")
  println("\tst\tyle C.line-width:\t"+ C.line_width.get)
  println("\tst\tyle C.transparency\t: "+ C.transparency.get)
  println("\tst\tyle C.font-size:\t"+ C.font_size.get)
  println("\tstyle D.font-size:\t "+ D.font_size.get + " <- even D has A's font-size, for C inherits from A and D inherits from C")
  println("""see, C has most relevant line-width = 7(from self), most relevant transparency 4(from most important parent B)
   and font-size = 100(from second most important parent A)""")


}
