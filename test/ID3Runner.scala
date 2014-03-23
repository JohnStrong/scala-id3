package test

import  util.Random
import  io.Source

import  id3._
import java.io.{FileWriter, PrintWriter}

object ID3Runner {

  private val FILE_PATH = "C:/owls.csv"

  private val ATTRIBUTE_LIST = List("bodyLength", "wingLength" ,"bodyWidth", "wingWidth")
  private val TARGET_ATTRIBUTE = "owlType"

  private val RINSE_REPEAT = 10

  def readFromCSV(fileName:String, separator:String = ",") = {
    val raw = Source.fromFile(fileName)
    raw.getLines().toSeq.map { l => l.split(separator) }
  }

  def split(f:Seq[Array[String]]) = {
    val len = f.length
    val third = Math.round(len/3)

    val shuffled = Random.shuffle(f.toList)

    val training = shuffled.slice(0, third * 2)
    val test = shuffled.slice(third * 2, len)

    (training, test)
  }

  def printTrainingTest(training:List[Array[String]], test:List[Array[String]]) {
    training.groupBy { t => t(0) }foreach{t => print(t._1 + ' ')}
    println
    test.groupBy { t => t(0) }.foreach{t => print(t._1 + ' ')}
  }

  // takes a decision tree and pretty prints the result
  def printTreeResult(tree:DecisionTreeNode) {
    println(tree.attribute)

    for(c <- tree.children) {
      println(c._1)
      printTreeResult(c._2)
    }

    println
  }

  def writeTo(f:String)(c:PrintWriter => Unit) {

    val pw = new PrintWriter(new FileWriter(f, true))

    try {
      c(pw)
    } finally {
      pw.close()
    }
  }

  // classify and computes average accuracy of algorithm
  // reading training and test data from file
  def perform = {
    val d = readFromCSV(FILE_PATH)
    val (training, test) = split(d)

    val data = training.map {
      e => id3.Data(e(0), e(1),e(2), e(3),e(4))
    }

    val t = test.map {
      e => id3.Data( e(0), e(1), e(2), e(3), e(4))
    }

    implicit val res = DecisionTree.create(data, ATTRIBUTE_LIST, TARGET_ATTRIBUTE)

    DecisionTree.classify(t, TARGET_ATTRIBUTE)(avg => avg)
  }

  def main(args:Array[String]) {
    val avgs = for(i <- 1 to RINSE_REPEAT) yield perform
    val avg = avgs.sum/RINSE_REPEAT

    avgs.zipWithIndex.foreach(a => print("run(" + a._2 + "):" + a._1 + '\t'))

    println
    println("average accuracy:\t" + avg)
  }
}
