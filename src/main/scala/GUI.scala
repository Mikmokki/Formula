import UIs.Functions.playMusic
import com.sun.javafx.application.PlatformImpl.addListener
import javafx.beans.binding.IntegerBinding
import javafx.scene.paint.ImagePattern
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene._
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, Text}

import java.io.File
import javax.sound.sampled.{AudioSystem, Clip}
import scala.math._
import scala.util.Random

object GUI extends JFXApp {
  val trackImage = "file:GUIResources/images/road.png"
  val outOfFieldImage = "file:GUIResources/images/grass.png"
  val flag = "file:GUIResources/images/flag.png"
  val trackAmount = Option(new File("data").list.length).getOrElse(0)
  val botPoints = Array.ofDim[(Int, Int)](4)
  val rightPane = new VBox() {
    this.spacing = 10
  }
  val screenSize = java.awt.Toolkit.getDefaultToolkit.getScreenSize
  var track = new Track(1)
  var formulas = collection.mutable.Map[Int, Formula]()
  var turn = 0
  var maxRound = 2
  var checkpoints = Array.ofDim[(Char, Int, Int)](4)
  var winner: Option[Formula] = None
  var gameOver = false

  stage = new JFXApp.PrimaryStage {
    title.value = "Formula game"
    val menuScene: Scene = new Scene(screenSize.width * 0.6, screenSize.height * 0.8) {
      val box = new VBox {
        this.spacing = 10
        this.alignment = Pos.TopCenter
        this.background = new Background(
          Array(
            new BackgroundImage(
              new Image(trackImage),
              BackgroundRepeat.NoRepeat,
              BackgroundRepeat.NoRepeat,
              BackgroundPosition.Center,
              new BackgroundSize(100, 100, true, true, true, true)
            )
          )
        )
        val playerOptions = ObservableBuffer("1 player", "2 players", "Player against a bot", "Bot Duel")
        val playerSpin = new Spinner[String]((playerOptions))
        val name1 = new TextField {
          maxWidth = 300
          prefHeight = 20
        }
        val name2 = new TextField {
          maxWidth = 300
          prefHeight = 20
        }
        val roundSpin = new Spinner[Int](1, 5, 1)
        val trackSpin = new Spinner[Int](1, trackAmount, 1)
        val playerLabel = new Label {
          text = "Select Players"
        }
        playerLabel.setTextFill(Color.White)
        val nameLabel1 = new Label {
          text = "Select Player 1 name"
        }
        nameLabel1.setTextFill(Color.White)
        val nameLabel2 = new Label {
          text = "Select Player 2 name"
        }
        nameLabel2.setTextFill(Color.White)
        val roundLabel = new Label {
          text = "Select round amount"
        }
        roundLabel.setTextFill(Color.White)
        val trackLabel = new Label {
          text = "Select track"
        }
        trackLabel.setTextFill(Color.White)

        val header = new Label("THE FORMULA GAME")
        header.padding = Insets(50, 25, 10, 25)
        header.font = Font(screenSize.height / 10)
        header.setTextFill(new ImagePattern(new Image(outOfFieldImage)))

        val buttonContainer = new VBox
        buttonContainer.padding = Insets(25, 100, 25, 100)
        buttonContainer.spacing = 10
        buttonContainer.alignment = Pos.TopCenter

        val playButton = new Button {
          text = "Play Game"
          maxWidth = 300
          prefHeight = 50
          onAction = e => {
            maxRound = roundSpin.value.get()
            track = new Track(trackSpin.value.get())
            track.loadTrack()
            val n1 = if (name1.getText.isEmpty) "Player 1" else name1.getText
            val n2 = if (name2.getText.isEmpty) "Player 2" else name2.getText
            playerSpin.value.get() match {
              case a if (a == playerOptions(0)) => {
                formulas += 1 -> new Formula(0, 0, n1, track.map)
              }
              case b if (b == playerOptions(1)) => {
                formulas += 1 -> new Formula(0, 0, n1, track.map)
                formulas += 2 -> new Formula(1, 1, n2, track.map)
              }
              case c if (c == playerOptions(2)) => {
                formulas += 1 -> new Formula(0, 0, n1, track.map)
                formulas += 2 -> new AI(1, 1, "SUPER-AI", track.map)
              }
              case d if (d == playerOptions(3)) => {
                formulas += 1 -> new AI(0, 0, "SUPER-AI 1", track.map)
                formulas += 2 -> new AI(1, 1, "SUPER-AI 2", track.map)
              }
              case _ => println("Something wrong with the game set up")
            }
            gameScene.iniatilizeGame()
            rightPane.children(5) = refresh()
            stage.scene = gameScene
            stage.maximized = true
          }
        }
        buttonContainer.children = Array(playerLabel, playerSpin, nameLabel1, name1, nameLabel2, name2, roundLabel, roundSpin, trackLabel, trackSpin, playButton)
        this.children = Array(header, buttonContainer)
      }
      root = box
    }
    // lazy as the track will be loaded after menu as track and other things need to be selected first.
    lazy val gameScene = new Scene(screenSize.width, screenSize.height) {
      val border = new BorderPane
      var grid = new GridPane()
      val gridSize = screenSize.width / 70
      var dirChange = ""
      var gearChange = ""
      var xMax: Int = 0
      var yMax: Int = 0
      val leftPane:VBox = new VBox {
        this.spacing = 10
        val newButton = new Button {
          text = "New Game"
          prefWidth = 100
          onMouseClicked = e => {
            stage.scene = menuScene
            stage.maximized = false
            formulas = formulas.empty

          }
        }
        val restartButton = new Button {
          prefWidth = 100
          text = "Restart Game"
          onMouseClicked = e => {
            formulas.values.foreach(_.restartStats())
            iniatilizeGame()
            rightPane.children(5) = refresh()
          }
        }
        val list = new ListView(track.scoreboard.toList)
        list.setPrefHeight(list.getItems.size * 24)
        children = Seq(newButton, restartButton, new Label {
          text = "Fastest laps"
          style = "-fx-font-size: 20pt"
        }, list, new TextArea {
          text = "Move by selecting gear\nand direction. Then\npress Move Player."
          disable = true
          style = "-fx-font-size: 12pt"
        })
        prefWidth = 200
        minWidth = 100
        prefHeight =  gridSize*yMax-list.getPrefHeight
      }
      border.left = leftPane

      def iniatilizeGame() = {
        gameOver = false
        xMax = math.max(track.map(0).length - 1, 0)
        yMax = math.max(track.map.size - 1, 0)
        turn = 0
        winner = None
        grid.getChildren.clear()
        var rowCounter = 0
        var charCounter = 0
        for (row <- track.map) {
          for (char <- row) {
            var image = new ImageView(trackImage)
            char match {
              case '#' => image = new ImageView(outOfFieldImage)
              case '1' => {
                formulas(1).x = charCounter
                formulas(1).y = rowCounter
                formulas(1).image = "file:GUIResources/images/player1.png"
                formulas(1).color = "Red"
                image = new ImageView(trackImage)
              }
              case '2' if (formulas.size >= 2) => {
                formulas(2).x = charCounter
                formulas(2).y = rowCounter
                formulas(2).image = "file:GUIResources/images/player2.png"
                formulas(2).color = "Blue"
                image = new ImageView(trackImage)
              }
              case 'R' => image = {
                checkpoints(0) = (char, charCounter, rowCounter)
                new ImageView(outOfFieldImage)
              }
              case 'D' => image = {
                checkpoints(1) = (char, charCounter, rowCounter)
                new ImageView(outOfFieldImage)
              }
              case 'L' => image = {
                checkpoints(2) = (char, charCounter, rowCounter)
                new ImageView(outOfFieldImage)
              }
              case 'G' => image = {
                checkpoints(3) = (char, charCounter, rowCounter)
                new ImageView(flag)
              }
              case 'Z' => image = {
                botPoints(0) = (charCounter, rowCounter)
                new ImageView(trackImage)
              }
              case 'X' => image = {
                botPoints(1) = (charCounter, rowCounter)
                new ImageView(trackImage)
              }
              case 'C' => image = {
                botPoints(2) = (charCounter, rowCounter)
                new ImageView(trackImage)
              }
              case 'V' => image = {
                botPoints(3) = (charCounter, rowCounter)
                new ImageView(trackImage)
              }
              case _ => image = new ImageView(trackImage)
            }
            image.fitWidth = gridSize
            image.fitHeight = gridSize
            grid.add(image, charCounter, rowCounter)
            charCounter += 1
          }
          charCounter = 0
          rowCounter += 1
        }
        for (f <- formulas.values) {
          f match {
            case ai: AI => ai.botPoints = botPoints
            case _ =>
          }
          val image = new ImageView(f.image)
          image.fitHeight = gridSize
          image.fitWidth = gridSize
          grid.add(image, f.x.round.toInt, f.y.round.toInt)
        }
        val shuf = Random.shuffle(List(1, 2))
        if (formulas.size == 2 && shuf.head == 1) {
          val f = formulas(1)
          val f2 = formulas(2)
          formulas(1) = formulas(2)
          formulas(2) = f
          formulas(1).image = f2.image
          formulas(2).image = f.image
          formulas(1).y = f2.y
          formulas(2).y = f.y
          formulas(1).color = f2.color
          formulas(2).color = f.color
        }
        leftPane.children(3) = new ListView(track.scoreboard.toList)
      }

      val moveB = new Button {
        text = "Move Player"
        onMouseClicked = e => {
          playMusic("guiresources/sounds/move.wav")
          val formula = formulas(turn % formulas.size + 1)
          if (gameOver) {
            new Alert(AlertType.Information) {
              title = "New Game?"
              contentText = "Start a new game by pressing the restart game button"
              showAndWait()
            }
          }
          else if (!formula.gameOver) {
            val image = if (track.map(math.max(math.min(formula.y.round.toInt, yMax), 0))(math.max(math.min(formula.x.round.toInt, xMax), 0)) == '#') new ImageView(outOfFieldImage) else new ImageView(trackImage)
            image.fitWidth = gridSize
            image.fitHeight = gridSize
            grid.add(image, math.max(math.min(formula.x.round.toInt, xMax), 0), math.max(math.min(formula.y.round.toInt, yMax), 0))
            formula.turn += 1
            formula.move(gearChange, dirChange)

            if (formula.x.round.toInt > checkpoints(0)._2 && formula.y.round.toInt > checkpoints(0)._3) {
              formula.checks(0) = true
            } else if (formula.x.round.toInt < checkpoints(1)._2 && formula.y.round.toInt > checkpoints(1)._3 && formula.checks(0)) {
              formula.checks(1) = true
            } else if (formula.x.round.toInt < checkpoints(2)._2 && formula.y.round.toInt < checkpoints(2)._3 && formula.checks(0) && formula.checks(1)) {
              formula.checks(2) = true
            } else if (formula.x.round.toInt > checkpoints(3)._2 && formula.y.round.toInt < checkpoints(3)._3 && formula.checks(0) && formula.checks(1) && formula.checks(2)) {
              formula.round += 1
              formula.currentTurn = formula.turn - formula.currentTurn
              if (formula.currentTurn < track.scoreboard(9)._2) {
                track.scoreboard(9) = (formula.owner, formula.currentTurn)
                track.scoreboard = track.scoreboard.sortBy(_._2)
                track.saveTrack()
                leftPane.children(3) = new ListView(track.scoreboard.toList)
              }
              formula.round match {
                case r if (r == maxRound) => {
                  formula.checks(3) = true
                  formula.gameOver = true
                  if (winner.isEmpty) {
                    playMusic("guiresources/sounds/victory.wav")
                    winner = Some(formula)
                    new Alert(AlertType.Information) {
                      title = "Winner Winner Chicken Dinner"
                      headerText = winner.get.owner + " Won The Game!"
                      contentText = "Turns " + winner.get.turn
                      showAndWait()
                    }
                  }
                  if (formulas.forall(_._2.gameOver)) {
                    gameOver = true
                    new Alert(AlertType.Information) {
                      title = "Game Over"
                      headerText = winner.get.owner + " Won The Game with " + winner.get.turn + " turns!"
                      contentText = "Others: " + formulas.values.filterNot(_ == winner.get).map(x => x.owner + ": " + x.turn + " turns").mkString(" ")
                      showAndWait()
                    }
                  }
                }
                case _ => {
                  formula.checks = Array(false, false, false, false)
                }
              }
            }
            if (formula.route.exists(x => track.map(x._2)(x._1) == '#')) {
              playMusic("guiresources/sounds/crash.wav")
              val cords = formula.route.reverse.find(x => track.map(x._2)(x._1) != '#').get
              formula.setPenalty(cords._1, cords._2)
              formula.dir += Pi
            }
            for (f <- formulas.values) {
              if (formula.x.round == f.x.round && formula.y.round == f.y.round && f != formula) {
                playMusic("guiresources/sounds/horn.wav")
                val image = new ImageView(trackImage)
                val oldX = f.x.round.toInt
                val oldY = f.y.round.toInt
                image.fitWidth = gridSize
                image.fitHeight = gridSize
                grid.add(image, oldX, oldY)
                formula.dir match {
                  case d if (d > 0 && d <= Pi / 2) => f.x += 1
                  case d if (d > Pi / 2 && d <= Pi) => f.y -= 1
                  case d if (d > Pi && d <= Pi * 3 / 2) => f.x -= 1
                  case d if (d > Pi * 3 / 2 && d < Pi * 2 || d == 0) => f.y += 1
                }
                if (track.map(f.y.round.toInt)(f.x.round.toInt) == '#') {
                  playMusic("guiresources/sounds/crash.wav")
                  f.setPenalty(oldX, oldY)
                  f.dir += Pi
                }
                val image2 = new ImageView(f.image)
                image2.fitWidth = gridSize
                image2.fitHeight = gridSize
                image2.rotate = -(f.dir / Pi) * 180
                grid.add(image2, f.x.round.toInt, f.y.round.toInt)
              }
            }
            gearChange = ""
            dirChange = ""
            val image2 = new ImageView(formula.image)
            image2.fitWidth = gridSize
            image2.fitHeight = gridSize
            image2.rotate = -(formula.dir / Pi) * 180
            grid.add(image2, math.max(math.min(formula.x.round.toInt, xMax), 0), math.max(math.min(formula.y.round.toInt, yMax), 0))
            turn += 1
            rightPane.children(5) = refresh()
            for (b <- Vector(up, down, left, right)) {
              b.selected = false
            }
          } else {
            turn += 1
            rightPane.children(5) = refresh()
          }
        }
      }


      val up: ToggleButton = new ToggleButton {
        onMouseClicked = e => if (selected.value) {
          down.selected = false
          gearChange = "up"
        } else gearChange = ""
        text = "Gear Up"
      }
      val down: ToggleButton = new ToggleButton {
        onMouseClicked = e => if (selected.value) {
          up.selected = false
          gearChange = "down"
        } else gearChange = ""
        text = "Gear Down"
      }
      val left: ToggleButton = new ToggleButton {
        onMouseClicked = e => {
          if (selected.value) {
            right.selected = false
            dirChange = "left"
          } else dirChange = ""
        }
        text = "Turn Left"
      }
      val right: ToggleButton = new ToggleButton {
        onMouseClicked = e => {
          if (selected.value) {
            left.selected = false
            dirChange = "right"
          } else dirChange = ""
        }
        text = "Turn Right"
      }
      for (b <- Vector(up, down, left, right, moveB)) {
        b.prefHeight = 50
        b.prefWidth = 100
        rightPane.children += b
      }
      val textInfo = refresh()
      rightPane.children += textInfo
      border.right = rightPane
      border.center = grid
      root = border
    }
    def refresh() = {
      val num = turn % formulas.size + 1
      val form = formulas(num)
      val pen = if (form.penalty > 0) "\nPenalty: " + form.penalty + " rounds" else ""
      val gear = "\nGear " + form.gear + "/5"
      new Text("Player: " + form.owner + "\nTurn " + form.turn + pen + gear + " (" + form.color + ")")
    }



    scene = menuScene
  }

}
