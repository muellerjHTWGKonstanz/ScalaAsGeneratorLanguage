package model.shapecontainer.shape.geometrics.layouts

/**
 * Created by julian on 15.10.15.
 * the commonLayout (from the Grammarsheet)
 */
trait CommonLayout {
  val position:Option[(Int,Int)] // (x,y) Tuple
  val size_width:Int
  val size_height:Int

  /*unsafe getter!*/
  def x = position.get._1
  def y = position.get._2
}
