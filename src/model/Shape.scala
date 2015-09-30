package model

/**
 * Created by julian on 29.09.15.
 */
case class Shape(name:String = "no name",
                 key: Long = 0L,
                 style:Option[Style] = None,
                 extendedShape:Option[Shape] = None)
/*TODO the actual attributes are missing*/
