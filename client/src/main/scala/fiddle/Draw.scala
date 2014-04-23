package fiddle

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import fiddle._
import scala.collection.mutable

/**
 *
 * User: chester
 * Date: 4/23/14
 * Time: 12:40 PM
 */

case class Point(x: Double, y: Double){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = Point(x - p.x, y - p.y)
  def *(d: Double) = Point(x * d, y * d)
  def /(d: Double) = Point(x / d, y / d)
  def length = Math.sqrt(x * x + y * y)
}


object Draw {

  @JSExport
  def canvas = Util.getElem[dom.HTMLCanvasElement]("canvas")
  
  @JSExport
  def renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]


  case class Operator(name: String, pos: Point){
    val width = 20
    val height= 20
    val toLinks = mutable.ListBuffer.empty[Operator]

    def draw() = {

      renderer.strokeStyle = "white"
      renderer.lineWidth = 3
      renderer.strokeRect(pos.x, pos.y, width, height)

      renderer.fillStyle = "white"
      renderer.fillText(name, pos.x - 1, pos.y + 35)
    }

    def link(op: Operator) :Unit = {
      toLinks += op
    }

    def drawLinks() = {
      toLinks.foreach(this.drawLink)
    }

    private def drawLink(op: Operator) = {
      // Let's translate the drawing to this operator position
      // renderer.translate( pos.x, pos.y)

      // Set the stroke pattern to red
      renderer.strokeStyle = "#FF0000"

      // Set the fill pattern to grey
      renderer.fillStyle = "grey"
      // Reset the path
      renderer.beginPath()

      // Let's move to our starting point
      renderer.moveTo( pos.x + width, pos.y)

      // Let's draw our triangle lines
      renderer.lineTo( op.pos.x , op.pos.y + op.height)
      renderer.stroke()
      renderer.fill()

      // Close the path
      renderer.closePath()
    }

  }
  case class Flow (name: String, `type`: String) {
    val ops = mutable.ListBuffer.empty[Operator]
    def addOperator(op: Operator) {
      ops += op
    }

    def show() {
      ops.foreach(_.draw())
      ops.foreach(_.drawLinks())
    }

    def run() {

    }
  }
}

  @JSExport
  object ScalaJSExample {

    import Draw._
    var startPoint = Point(dom.innerWidth.toInt/2 -500 , dom.innerHeight.toInt/2 -50 )
    import Draw._
    var startPoint = Point(dom.innerWidth.toInt/2 -500 , dom.innerHeight.toInt/2 -50 )

    def draw() = {
      renderer.clearRect(0, 0, canvas.width, canvas.height)
      val op1 = Operator("data",startPoint)
      val op2 = Operator("row filter",Point(startPoint.x +100, startPoint.y -50 ))
      op1.link(op2)

      val f = Flow("test", "database")
      f.addOperator(op1)
      f.addOperator(op2)
      f.show()

      var current = op2
      for ( i <- 0 until 5 ) {
        val op = Operator(s"row filter_$i", Point(current.pos.x + 10, current.pos.y -30 ))
        current.link(op)
        f.addOperator(op)
        current = op
        //println("i= " + i)
      }

      f.show()

    }



    @JSExport
    def main(args: Array[String] ): Unit = {
      draw()
    }
  }


