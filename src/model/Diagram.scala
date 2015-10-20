package model

import model.shapecontainer.shape.Shape
import model.style.Style

/**
 * Created by julian on 24.09.15.
 */
case class Diagram(styleHierarchy: ClassHierarchy[Style], shapeHierarchy:ClassHierarchy[Shape])


