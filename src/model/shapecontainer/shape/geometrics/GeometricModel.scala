package model.shapecontainer.shape.geometrics

import model.style.Style
import parser.Cache

/**
 * Created by julian on 15.10.15.
 * representation of all Geometrical Models like Line Polyline Rectangel
 * and so on
 */
abstract class GeometricModel (var parent:Option[GeometricModel] = None)

