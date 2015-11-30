package model.diagram.node

/**
 * Created by julian on 30.11.15.
 */
case class Shape() {

}

case class ShapeProperty(value:Any, key:String)
 // ('var' attribute=[ecore::EAttribute] '->' key = ShapeDslKey ( '(' value=XExpression ')' )?) | TODO
 // ('val' '['value=XExpression']' '->' key=ShapeDslKey)

case class ShapeCompartment(nestedShape:model.shapecontainer.shape.Shape,
                            value:Any,
                            compartment:String)
/*TODO keine ahnung ob das so stimmt*/
