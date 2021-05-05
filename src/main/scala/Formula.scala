import scala.collection.mutable._
import scala.math._

class Formula(var x: Double, var y: Double, var owner: String, val map: ArrayBuffer[String]) {
  var gear = 1
  var turn = 0
  var currentTurn = 0
  var round = 0
  var penalty = 0
  var image = "file:GUIResources/images/player1.png"
  var color="Red"
  var dir: Double = 0
  var route = Buffer[(Int, Int)]()
  var checks = Array(false, false, false, false)
  var backOnTrack:Option[(Int,Int)] = None
  val xMax = map(0).length -1
  val yMax = map.size -1
  var gameOver = false

  def restartStats() = {
    gear = 1
     turn = 0
    currentTurn = 0
    round = 0
    penalty = 0
    dir = 0
    route = Buffer[(Int, Int)]()
    checks = Array(false, false, false, false)
    backOnTrack = None
    gameOver = false
  }

  def move(gearChange: String, dirChange: String): Unit = {
    route = Buffer[(Int, Int)]()
    if (penalty == 0) {
      gearChange match {
        case "up" => gear = min(5, gear + 1)
        case "down" => gear = max(1, gear - 1)
        case _ =>
      }
      dirChange match {
        case "left" => dir += 2 * Pi / (8 * gear)
        case "right" => dir -= 2 * Pi / (8 * gear)
        case _ =>
      }
      if (dir >= 2 * Pi) dir -= 2 * Pi
      else if (dir < 0) dir += 2 * Pi
      val oldX = x.round.toInt
      val oldY = y.round.toInt
      y += gear * (-sin(dir))
      x += gear * cos(dir)
      makeRoute(oldX,oldY)
    }
    else {

      penalty -= 1
      if (penalty ==0){
      x = backOnTrack.get._1
      y = backOnTrack.get._2
      backOnTrack =None}
    }

  }
   def makeRoute(oldX:Int,oldY:Int): Unit ={

      route = route :+ (oldX, oldY)
      route = route :+ (math.max(math.min(x.round.toInt,xMax),0), math.max(math.min( y.round.toInt,yMax),0))

      def line(newx: Double) = ((oldY - y.round.toInt) / (oldX - x.round.toInt)) * (newx - oldX) + oldY

      if( oldX != x.round.toInt){
        for (xx <- (min(oldX,x.round.toInt)*50 to max(oldX,x.round.toInt)*50 by 1).map(_/50.0)) {
        val pair = (math.max(math.min(xx.toInt,xMax),0), math.max(math.min(line(xx).toInt,yMax),0))
        if (!route.contains(pair)) {
          route += pair
        }
      }
      } else {
        for(y <- min(oldY,y.round.toInt) to max(y.round.toInt,oldY)) {
          val pair = (math.max(math.min(oldX,xMax),0), math.max(math.min(y,yMax),0))
          if (!route.contains(pair)) {
          route += pair
        }
        }
      }

      route = route.filter(x=>x._1>=0 && x._2 >=0 && x._1<=xMax && x._2<=yMax)
  }
  def setPenalty(x:Int,y:Int) = {
    backOnTrack = Some(x,y)
    penalty = 3
    gear = 1
  }
}
