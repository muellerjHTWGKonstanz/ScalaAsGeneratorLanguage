package model

import model.shapecontainer.shape.Shape
import model.style.Style

/**
 * Created by julian on 24.09.15.
 * representation of a container element for shapes and styles
 */
case class HierarchyContainer(styleHierarchy: ClassHierarchy[Style], shapeHierarchy:ClassHierarchy[Shape])


