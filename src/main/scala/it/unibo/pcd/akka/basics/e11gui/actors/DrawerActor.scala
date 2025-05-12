package it.unibo.pcd.akka.basics.e11gui.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import it.unibo.pcd.akka.basics.e11gui.actors.DrawMessage.Rectangle
import it.unibo.pcd.akka.basics.e11gui.view.{DrawablePanel, DrawableRectangle, ElementFactory, SimpleDrawablePanel, SwingElementFactory}

import java.awt.Color
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.swing.SimpleSwingApplication
import scala.swing.event.MousePressed
import scala.swing.{Dimension, Frame, MainFrame, SimpleSwingApplication}
import scala.util.{Random, Success}
import scala.concurrent.duration.DurationInt

enum DrawMessage:
  case Rectangle(x: Int, y: Int, width: Int, height: Int)

object DrawerActor:
  def apply[G](panel: DrawablePanel[G], elementFactory: ElementFactory[G]): Behavior[DrawMessage] =
    Behaviors.receiveMessage:
      case DrawMessage.Rectangle(x, y, width, height) =>
        panel.addElement(elementFactory.createRectangle(x, y, width, height))
        Behaviors.same

enum ClickActorListener:
  case Click(x: Int, y: Int)

object ClickActor:
  def apply[G](panel: DrawablePanel[G], drawer: akka.actor.typed.ActorRef[DrawMessage]): Behavior[ClickActorListener] =
    Behaviors.setup: context =>
      panel.whenClicked { case (x, y) =>
        context.pipeToSelf(Future.successful((x, y))) {
          case Success((x, y)) => ClickActorListener.Click(x, y)
          case _ => ClickActorListener.Click(0, 0)
        }
      }
      Behaviors.receiveMessage:
        case ClickActorListener.Click(x, y) =>
          context.log.info(s"Click at $x, $y")
          drawer ! DrawMessage.Rectangle(x, y, 5, 5)
          Behaviors.same

object IndependentActor:
  def apply[G](panel: DrawablePanel[G], drawer: akka.actor.typed.ActorRef[DrawMessage]) : Behavior[(Int,Int)] =
    Behaviors.setup { ctx =>
      Behaviors.withTimers{ timers =>
           Behaviors.receiveMessage {
             case (x,y) =>
               ctx.log.info(s"point ${x},${y}")
               timers.startSingleTimer((Random.nextInt(200) + 100, Random.nextInt(200) + 100), 300.millis)
               drawer ! DrawMessage.Rectangle(x,y, 10, 10)
               Behaviors.same
           }
      }
    }


object MainActor:
  def apply[G](panel: DrawablePanel[G], factory: ElementFactory[G]): Behavior[Nothing] =
    Behaviors.setup: context =>
      val drawer = context.spawn(DrawerActor[G](panel, factory), "drawer")
      val clickActor = context.spawn(ClickActor(panel, drawer), "clickActor")
      val independentActor = context.spawn(IndependentActor(panel, drawer), "independentActor")
      independentActor ! (0,0)

      Behaviors.empty

object MyDrawingApp extends SimpleSwingApplication:
  val panel = SimpleDrawablePanel()
  val factory = SwingElementFactory()

  def top: Frame = new MainFrame:
    title = "Drawable Panel Example"
    preferredSize = new Dimension(400, 300)
    contents = panel

    // Adding some rectangles to the panel
  akka.actor.typed.ActorSystem(MainActor(panel, factory), "main")
