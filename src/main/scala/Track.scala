import java.io._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import mutable._

class Track(trackNum: Int) {
  var scoreboard: ArrayBuffer[(String, Int)] = ArrayBuffer()
  var map = ArrayBuffer[String]()

  def loadTrack(): Unit = {
    try {
      val fReader = new FileReader("data/track" + trackNum + ".txt")
      val lineReader = new BufferedReader(fReader)
      var currentline = lineReader.readLine().trim.toUpperCase
      while (currentline != null && (!currentline.startsWith("/ENDTRACK"))) {
        currentline = currentline.trim.toUpperCase
        if (currentline.startsWith("/TRACKNR")) {
          currentline = lineReader.readLine()
          while (!currentline.toUpperCase.startsWith("/TRACKRDY")) {
            map += currentline.trim.toUpperCase
            currentline = lineReader.readLine()
          }

        } else if (currentline.startsWith("/SCOREBOARD")) {
          currentline = lineReader.readLine()
          while (!currentline.startsWith("/SCORERDY")) {
            val name = currentline.split('.')(1).split(":")(0).trim
            val score = currentline.split(":")(1).trim.toInt
            scoreboard += ((name, score))
            currentline = lineReader.readLine()
          }
        }
        currentline = lineReader.readLine()
      }
    } catch {
      case e: Throwable => throw new Exception("Error with loading the track")
    }

  }

  def saveTrack(): Unit = {
    var fileContent = Buffer[String]()
    fileContent += "/TRACKNR" + trackNum
    fileContent = fileContent ++ map
    fileContent += "/TRACKRDY"
    fileContent += " "
    fileContent += "/SCOREBOARD"
    fileContent =fileContent ++ scoreboard.sortBy(_._2).zipWithIndex.map(x=>(x._2+1) + ". " + x._1._1 + ": " + x._1._2  )
    fileContent += "/SCORERDY"
    fileContent += " "
    fileContent += "/ENDTRACK"

    try {
        val fileWriter = new FileWriter("data/track" + trackNum + ".txt")
        val buffWriter = new BufferedWriter(fileWriter)
       try {
                    buffWriter.write(fileContent.mkString("\n"))
                } finally {
                    buffWriter.close()
                }
    } catch {
      case e: Throwable => throw new Exception("Error with saving the track")
    }
  }
}