package id3.util

sealed trait ID3Implicit

trait AttributeExtractor {

  // owl class and desired attribute
  def getField[T](d:T, a:String) = {
    d.getClass.getDeclaredField(a)
  }

  def getFieldValue[T](d:T, a:String) = {
    val field = getField(d, a)
    field.setAccessible(true)
    field.get(d).toString
  }
}

trait Implicits extends ID3Implicit with AttributeExtractor {
  implicit def fieldToValue[T](o:T) = new AnyRef {
    def fieldValue(a:String) = getFieldValue(o, a)
  }
}
