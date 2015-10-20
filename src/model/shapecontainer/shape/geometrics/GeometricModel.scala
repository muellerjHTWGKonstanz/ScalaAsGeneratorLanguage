package model.shapecontainer.shape.geometrics

import model.style.Style

/**
 * Created by julian on 15.10.15.
 * representation of all Geometrical Models like Line Polyline Rectangel
 * and so on
 */
abstract class GeometricModel (val style:Option[Style] = None, parent:Option[GeometricModel] = None)
