package model.diagram.node

/**
 * Created by julian on 30.11.15.
 * diagrams shape definition
 */
case class Shape(shape: model.shapecontainer.shape.Shape,
                 properties:List[ShapeProperty],
                 compartment:List[ShapeCompartment]){

}

case class ShapeProperty(value:String, key:String)
 // ('var' attribute=[ecore::EAttribute] '->' key = ShapeDslKey ( '(' value=XExpression ')' )?) | TODO
 // ('val' '['value=XExpression']' '->' key=ShapeDslKey)

case class ShapeCompartment(value:Any,
                            compartment:String)
/*TODO keine ahnung ob das so stimmt*/
