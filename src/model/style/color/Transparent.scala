package model.style.color

case object Transparent extends Color with ColorWithTransparency{
  override def getRGBValue = """transparent"""
}
