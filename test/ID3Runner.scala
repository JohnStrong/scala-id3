package test

import  util.Random
import  io.Source

import  id3._
import  id3.util.Write

object ID3Runner {

  private val FILE_PATH = "C:/owls.csv"
  private val WRITE_PATH = "./src/lib/results.txt"

  private val ATTRIBUTE_LIST = List("bodyLength", "wingLength" ,"bodyWidth", "wingWidth")
  private val TARGET_ATTRIBUTE = "owlType"

  private val RINSE_REPEAT = 10

  private def readFromCSV(fileName:String, separator:String = ",") = {
    val raw = Source.fromFile(fileName)
    raw.getLines().toSeq.map { l => l.split(separator) }
  }

  private def split(f:Seq[Array[String]]) = {
    val len = f.length
    val third = Math.round(len/3)

    val shuffled = Random.shuffle(f.toList)

    val training = shuffled.slice(0, third * 2)
    val test = shuffled.slice(third * 2, len)

    (training, test)
  }

  private def write(v:Any) = {
    Write.to(WRITE_PATH) {
      s => s.println(v)
    }
  }

  // classify and computes average accuracy of algorithm
  // reading training and test data from file
  private def perform = {
    val d = readFromCSV(FILE_PATH)
    val (training, test) = split(d)

    val data = training.map {
      e => Data(e(0), e(1),e(2), e(3),e(4))
    }

    val t = test.map {
      e => Data( e(0), e(1), e(2), e(3), e(4))
    }

    implicit val res = DecisionTree.create(data, ATTRIBUTE_LIST, TARGET_ATTRIBUTE)

    DecisionTree.classify(t, TARGET_ATTRIBUTE) {
      acc => {
        write("Accuracy:\t\t\t" + acc)
        acc
      }
    }
  }

  def main(args:Array[String]) {
    val avgs = for(i <- 1 to RINSE_REPEAT) yield perform
    val avg = avgs.sum/RINSE_REPEAT

    write("Average Accuracy:\t" + avg)
  }
}
