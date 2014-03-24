package id3.util

import java.io.{FileWriter, PrintWriter}

object Write {

  def to[B](p:String)(c:PrintWriter => B) = {
    val f = new PrintWriter(new FileWriter(p, true))

    try {
      c(f)
    } finally {
      f.close()
    }
  }
}
