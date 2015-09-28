package model

/**
 * Created by julian on 22.09.15.
 */

import java.awt.Color
case class Style(
                  name:String                   = "no Name",
                  key:Long                      = 1L,
                  description:Option[String]    = None,
                  transparency:Option[Double]   = None,
                  backgroundColor:Option[Color] = None,
                  lineColor:Option[Color]       = None,
                  lineStyle:Option[String]      = None,
                  lineWidth:Option[Int]         = None,
                  fontColor:Option[Color]       = None,
                  fontName:Option[String]       = None,
                  fontSize:Option[Int]          = None,
                  fontBold:Option[Boolean]      = None,
                  fontItalic:Option[Boolean]    = None,
                  gradientOrientation:Option[String] = None,

                  childOf:Option[Style] = None)
