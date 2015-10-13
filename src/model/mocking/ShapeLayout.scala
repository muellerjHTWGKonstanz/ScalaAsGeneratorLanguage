package model.mocking

/**
 * Created by julian on 01.10.15.
 */
case class ShapeLayout( size_min:Option[Size] = None,
                        size_max:Option[Size] = None,
                        stretching:Option[Stretching] = None,
                        proportional:Boolean = false)
