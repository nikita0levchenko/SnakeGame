import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.{Green, Red, White}
import scalafx.scene.shape.Rectangle

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object SnakeFx extends JFXApp3 {

  val initialSnake: List[(Double, Double)] = List((200, 200), (225, 200), (250, 200))

  def randomFood(): (Double, Double) = (Random.nextInt(24) * 25, Random.nextInt(24) * 25)

  def rect(xr: Double, yr: Double, color: Color): Rectangle = new Rectangle {
    x = xr
    y = yr
    width = 25
    height = 25
    fill = color
  }

  case class State(snake: List[(Double, Double)], food: (Double, Double)) {

    def rectangles(): List[Rectangle] = rect(food._1, food._2, Red) :: snake.map {
      case (x, y) => rect(x, y, Green)
    }

    def newState(direction: Int): State = {
      val (x, y) = snake.head
      val (newX, newY) = direction match {
        case 1 => (x - 25, y)
        case 2 => (x + 25, y)
        case 3 => (x, y - 25)
        case 4 => (x, y + 25)
        case _ => (x, y)
      }

      val newSnake: List[(Double, Double)] =
        if (newX < 0 || newX >= 600 || newY < 0 || newY >= 600 || snake.tail.contains((newX, newY)))
          initialSnake
        else if (food == (newX, newY))
          food :: snake
        else
          (newX, newY) :: snake.init

      val newFood: (Double, Double) =
        if ((newX, newY) == food)
          randomFood()
        else
          food

      State(newSnake, newFood)
    }
  }

  def gameLoop(update: () => Unit): Unit = Future {
    update()
    Thread.sleep(1000 / 25 * 2)
  }.flatMap(_ => Future(gameLoop(update)))

  override def start(): Unit = {
    val state = ObjectProperty(State(initialSnake, randomFood()))
    val frame = IntegerProperty(0)
    val direction = IntegerProperty(4)

    frame.onChange {
      state.update(state.value.newState(direction.value))
    }

    stage = new JFXApp3.PrimaryStage {
      width = 600
      height = 600
      scene = new Scene {
        fill = White
        content = state.value.rectangles()
        onKeyPressed = key => key.getText match {
          case "w" => direction.value = 3
          case "s" => direction.value = 4
          case "a" => direction.value = 1
          case "d" => direction.value = 2
        }

        frame.onChange {
          Platform.runLater { () => {
            content = state.value.rectangles()
          }
          }
        }
      }
    }

    gameLoop(() => frame.update(frame.value + 1))
  }
}
