package id3

import  scala.collection.mutable.{HashMap, ListBuffer}
import  scala.language.{reflectiveCalls, implicitConversions}

sealed trait ID3

case class Data(
  bodyLength:String,
  wingLength:String,
  bodyWidth:String,
  wingWidth:String,
  owlType:String
) extends ID3

case class DecisionTreeNode(
  attribute:String,
  children:ListBuffer[(String, DecisionTreeNode)]
) extends ID3

object DataAttributeExtractor extends ID3 {

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

object DataImplicits extends ID3 {
  implicit def fieldToValue[T](o:T) = new AnyRef {
    def fieldValue(a:String) = DataAttributeExtractor.getFieldValue(o, a)
  }
}

object AttributeSelector extends ID3 {

  import DataImplicits.fieldToValue

  // d -> set of filtered data
  // t -> target attribute
  private def entropy[T](d:List[T], t:String) = {
    val freq = HashMap[String, Double]().empty

    // calculate frequency of each target attribute value in d
    d.foreach {
      e =>
        freq.find {
          entry => entry._1 == e.fieldValue(t)
        }.map {
          entry => entry._1 -> (entry._2 + 1.0)
        }.getOrElse {
          freq += (e.fieldValue(t) -> 1.0)
        }
    }

    // calculate entropy of data for each target attribute value
    val eS = freq.map {
      f => {
        val e = (-f._2/d.length) * Math.log(f._2/d.length)/Math.log(2)
        (f._1, e)
      }
    }

    eS.foldLeft(0.0) { _ + _._2}
  }

  // d -> data set
  // a -> current attribute
  // t -> target attribute
  private def gain[T](d:List[T], a:String, t:String) = {
    val freq = HashMap[String, Double]().empty

    // store all subset values of the current attribute a
    d.foreach {
      owl =>
        freq.find {
          entry => entry._1 == owl.fieldValue(a)
        }.map {
          e => (e._1 ,e._2 + 1.0)
        }.getOrElse {
          freq += (owl.fieldValue(a) -> 1.0)
        }
    }

    // calculate the entropy of the current attribute
    val eS = freq.map {
      entry => {
        val p = entry._2 / freq.values.sum
        val dP = d.filter(e => e.fieldValue(a) == entry._1)
        (entry._1, p * entropy(dP, t))
      }
    }

    // subtract the entropy from current attribute
    // from entropy of the entire set
    entropy(d, t) - eS.foldLeft(0.0) { _ + _._2 }
  }

  def getAttributeValues[T](d:List[T], a:String) = {
    d.groupBy(e => e.fieldValue(a))
  }

  // return set of data in 'd' that has attr value 'v' for 'a'
  def filterForVal[T](d:List[T], a:String, v:String) = {
    d.filter { e => v == e.fieldValue(a) }
  }

  def getMajorityTarget[T](d:List[T], t:String) = {
    val groups = getAttributeValues(d, t)
    groups.maxBy(e => e._2.length)
  }

  // chooses an attribute in a best suited to classify some data d
  def selectBestAttribute[T](d:List[T], a:List[String], t:String) = {
    val g = a.map {
      attr => (attr, gain(d, attr, t))
    }

    g.maxBy { _._2 }._1
  }
}

object DecisionTree extends ID3 {

  import DataImplicits.fieldToValue

  private def _classify[T](
    d:List[T],
    t:String
  )(implicit tree:DecisionTreeNode):Double = {

    val best = tree.attribute

    if(tree.children.isEmpty && d.isEmpty) {
      0.0
    } else if(tree.children.isEmpty && !d.isEmpty) {
      d.map {
        case e if e.fieldValue(t) == best => 1.0
        case _ => 0.0
      }.sum
    } else {

      // group all results by best attribute
      val m = d.groupBy {
        e => e.fieldValue(best)
      }

      m.map {
        kv => {

          val s = try {
            // gets minimum in C4.5 style classification for better accuracy
            tree.children.minBy {
              c => {
                Math.abs(c._1.toDouble - kv._1.toDouble)
              }
            }
          } catch {
            case e:Exception => tree.children.find(c => c._1 == kv._1).get
          }

          _classify(kv._2, t)(s._2)
        }
      }.sum
    }
  }

  // d -> data set of owl attribute values
  // a -> the attribute list
  // t -> the target attribute
  def create[T](d:List[T], a:List[String], t:String):DecisionTreeNode = {

    val (attr, vals) = AttributeSelector.getMajorityTarget(d, t)

    if(d.isEmpty || a.isEmpty) {
      DecisionTreeNode("", ListBuffer[(String, DecisionTreeNode)]())
    } else if(vals.length == d.length) {
      DecisionTreeNode(attr, ListBuffer[(String, DecisionTreeNode)]())
    } else {

      // get the attribute that best classifies the data set 'd'
      val best = AttributeSelector.selectBestAttribute(d, a, t)

      // create a new decision tree root node
      val root = DecisionTreeNode(best, ListBuffer[(String, DecisionTreeNode)]())

      val attrs = AttributeSelector.getAttributeValues(d, best)

      // get all unique attribute values for best
      val attrKeys = attrs.keys

      // for each value of the best attribute create a child node with result
      attrKeys.foreach {
        v => {

          val fv =  AttributeSelector.filterForVal(d, best, v)

          val child = create(fv, a.filterNot { attr => attr == best }, t)

          // append the result to the tree root node
          root.children += Tuple2[String, DecisionTreeNode](v, child)
        }
      }

      // return the entire tree
      root
    }
  }

  // d -> test data
  // t -> target attribute
  // _after -> function callback that takes the computed classification accuracy
  // tree -> decision tree from which to classify t
  def classify[T, A](d:List[T],t:String)(_after:Double => A)(implicit tree:DecisionTreeNode) = {
    val l = d.length
    val r = _classify(d, t)

    _after(r/l)
  }
}
