import java.util.Scanner

import model.diagram.Diagram
import model.style.Style
import parser.SprayParser

/**
 * Created by julian on 03.02.16.
 */
object performanceTest extends App{
  val standardStyleHead = (name:Int) => s"style Style$name "
  val standardStyleBody = """{
                  description = "The default style of the petrinet hierarchyContainer type."
                  transparency = 0.95
                  background-color = gradient fooGradient {
                    description = "default Gradient Description"
                    area ( color = blue, offset = 2.2)
                    area ( color = transparent, offset = 2.2)
                  }
                  line-color = black
                  line-style = solid
                  line-width = 1
                  font-color = blue
                  font-name = "Tahoma"
                  font-size = 6
                  font-bold = yes
                  font-italic = yes
                  gradient-orientation = horizontal
                 }"""
  val standardShapeHead = (name:Int) => s"shape Shape$name "
  val standardShapeBody = """{
                     size-min (width=4, height=6)
                     size-max (width=10, height=11)
                     stretching (horizontal=true, vertical=false)
                     proportional = true
                     text{
                       size(width=10, height=40)
                       id = Hallo1
                       textBody = "standard text body"
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
                     description {
                       align (horizontal=center, vertical=top)
                       id = BABABA
                     }
                     anchor = center
                 }"""

  val standardDiagramHead = (name:Int) => s"diagram Diagram$name for mock"
  val standardDiagramBody = (name:Int) => s"""{
                  actionGroup actGrp1 {
                     action act1 ( label : foo.foo.foo , method : fooImpl1, class : Foo)
                     action act2 ( label : foo.foo.foo , method : fooImpl2, class : Foo)
                     action act4 ( label : foo.foo.foo , method : fooImpl4, class : Foo)
                  }
                  node fooNode for mock {
                     shape : Shape$name (val testText -> Hallo1)
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
                 }"""

  val parser = new SprayParser()
  val lowerBound = 1
  val upperBound = 10000

  private def performanceTestStyleShape(parse:(String) => List[Any], generateHead:(Int) => String, body:String) = {
    val timeStampStart = System.nanoTime()
    for(i <- lowerBound to upperBound)
      parse(generateHead(i)+body)
    System.nanoTime() - timeStampStart
  }

  private def performanceTestDiagram = {
    for(i <- lowerBound to upperBound) yield
      parser.parseRawDiagram(standardDiagramHead(i)+standardDiagramBody(i))
  }


  while (new Scanner(System.in).hasNext){
    performanceTestStyleShape(parser.parseRawShapeSketch, standardShapeHead, standardShapeBody)
    performanceTestDiagram
    println("done")
  //  println({val stamp = performanceTest(parser.parseRawStyle, standardStyleHead, standardStyleBody); s"took ${stamp}ns (~${stamp/1000000}ms), to create $upperBound Styles"})
  }

}
