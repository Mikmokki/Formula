import scala.collection.mutable._
import scala.math._

class AI(botX: Double,botY: Double, botName: String, map: ArrayBuffer[String]) extends Formula(botX,botY,botName,map){
  var pointBoolean:Array[Boolean] = Array(false, false, false, false)
  var botPoints = Array.ofDim[(Int, Int)](4)
  override def restartStats() = {
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
    pointBoolean = Array(false, false, false, false)
   botPoints = Array.ofDim[(Int, Int)](4)
  }

   override def move(gearChange: String, dirChange: String): Unit = {
     route = Buffer[(Int, Int)]()
    if (penalty == 0) {
      pointBoolean match {
        case Array(false, false, false, false) => {
          dir match {
            case dirRight if (dir >= 3*Pi/16 && dir <=Pi) =>check("up","right")
            case dirLeft if (dir <= 29*Pi/16 && dir >=Pi)=>check("up","left")
              case j if (y.round.toInt-botPoints(0)._2)>=1 => check("up","left")
            case i if (abs(botPoints(0)._2-y.round.toInt)>=2) => check("up","right")
              case _ => check("up","")
          }
          if (abs(botPoints(0)._1-x)<=2) {
            pointBoolean(0) = true
          }
        }
          case Array(true, false, false, false) => {
          dir match {
            case dirRight if (dir >= 27*Pi/16 || dir <=Pi/2) =>check("up","right")
            case dirLeft if (dir <= 21*Pi/16 && (dir >=Pi/2 ))=>check("up","left")
             case j if (botPoints(1)._1-x.round.toInt)>=1 => check("up","left")
               case i if (abs(botPoints(1)._1-x.round.toInt)>=2) => check("up","right")
              case _ => check("","")
          }
            if (abs(botPoints(1)._2-y.round.toInt)<=2) {
            pointBoolean(1) = true
          }
          }
          case Array(true, true, false, false) =>{
          dir match {

            case dirRight if (dir >= 19*Pi/16 )=>check("up","right")
            case dirLeft if (dir <= 13*Pi/16) =>check("up","left")
               case j if (botPoints(2)._2-y.round.toInt)>=1 => check("up","left")
               case i if (abs(botPoints(2)._2-y.round.toInt)>=2) => check("up","right")
              case _ => check("up","")
          }
            if (abs(botPoints(2)._1-x.round.toInt)<=2) {
            pointBoolean(2) = true
          }
          }
          case Array(true, true, true, false) => {
          dir match {
            case dirRight if (dir >= 11*Pi/16 && dir <=3*Pi/2) =>check("up","right")
            case dirLeft if (dir <= 5*Pi/16 || dir >=3*Pi/2 )=>check("up","left")
             case j if (x.round.toInt-botPoints(3)._1)>=1 => check("up","left")
               case i if (abs(botPoints(3)._1-x.round.toInt)>=2) => check("up","right")
              case _ => check("","")
          }
            if (abs(botPoints(3)._2-y.round.toInt)<=2) {
            pointBoolean = Array(false, false, false, false)
          }}
        case _ => println("Bot is broken")
      }
    }
     else {
      penalty = (penalty -1)
      if (penalty ==0){
      x = backOnTrack.get._1
      y = backOnTrack.get._2
      backOnTrack =None}
    }
      }




 private def check(gearChange: String, dirChange: String): Unit = {
    route = Buffer[(Int, Int)]()
      gearChange match {
        case "up" => gear = min(2, gear + 1)
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
}
