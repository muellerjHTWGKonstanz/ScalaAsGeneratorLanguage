import util.ClassHierarchyOrderedByString

/**
 * Created by julian on 9/4/15.
 */
object testing extends App {

  var classes = new ClassHierarchyOrderedByString
  classes newBaseClass "A"
  classes newBaseClass "F"
  classes("A", "B")
  classes("A", "D")
  classes("B", "C")
  classes("C", "E")
  classes("F", "E")

  println(classes("A").depth)
  println(classes("B").depth)
  println(classes("C").depth)
  println(classes("E").depth)

  classes.root.rPrint()
  println("------------------------------------------------------------------------------")

  println("font-size".matches("font.size"))

  val map = Map("hi" -> 12)
  println(map("hi"))
  var list = List(1, 2, 3, 4)
  println(list map (x => x.toString -> x))

  val stringCriteria = "[A-Z]+[a-z]+"
  val stri = "Tahoma"
  stri match {
    case x if x.matches(stringCriteria) => println("success")
    case x => println("fail" + x.getClass)
  }
}

