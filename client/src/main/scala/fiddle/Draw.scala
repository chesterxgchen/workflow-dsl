package fiddle



import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.collection.mutable
import scala.async.Async._
import org.scalajs.dom.extensions.Ajax
import scala.async.Async.{async, await}
import scala.concurrent.{Promise, Future}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import fiddle._


import scalatags._
import scalatags.all._
import scala.scalajs.js.Dynamic._
import scala.Some

import Page.{red, green, blue}

object Client2 {

  def logspam = Util.getElem[dom.HTMLPreElement]("logspam")
  var logged = div()

  def logln(s: Modifier*): Unit = {
    log(div(s:_*))
  }

  def log(s: Modifier*): Unit = {
    logged = s.foldLeft(logged)((a, b) => b.transform(a))

    logspam.innerHTML = logged.toString()
    logspam.scrollTop = logspam.scrollHeight - logspam.clientHeight
  }

  def compile2(code: String, endpoint: String): Future[(Option[String], Option[String])] = async {
    if (code == "") (None, None)
    else {
      log(s"Compiling with $endpoint... ")
      val res = await(Ajax.post(endpoint, code))
      val result = JsVal.parse(res.responseText)
      log(result.asString)
      if (result("logspam").asString != ""){
        logln(result("logspam").asString)
      }
      if(result("success").asBoolean) log(green("Success"))
      else log(red("Failure"))

      (result.get("code").map(_.asString), result.get("output").map(_.asString))
    }
  }

  def evalDslWithSrc(src: String ): Unit = async {
    val (_, output) = await(compile2(src, "/evalDsl"))
    output.foreach( Page.println(_))
  }
}

//need uncomment when use
/*
@JSExport
class JsVal(val value: js.Dynamic){
  def get(name: String): Option[JsVal] = {
    (value.selectDynamic(name): js.Any) match {
      case _: js.Undefined => None
      case v => Some(JsVal(v))
    }
  }
  def apply(name: String): JsVal = get(name).get
  def apply(index: Int): JsVal = value.asInstanceOf[js.Array[JsVal]](index)

  def keys: Seq[String] = js.Object.keys(value.asInstanceOf[js.Object]).toSeq.map(x => x: String)
  def values: Seq[JsVal] = keys.toSeq.map(x => JsVal(value.selectDynamic(x)))

  def isDefined: Boolean = !(value: js.Any).isInstanceOf[js.Undefined]
  def isNull: Boolean = value eq null

  def asDouble: Double = value.asInstanceOf[js.Number]
  def asBoolean: Boolean = value.asInstanceOf[js.Boolean]
  def asString: String = value.asInstanceOf[js.String]

  override def toString(): String = js.JSON.stringify(value)
}

@JSExport
object JsVal {
  implicit def jsVal2jsAny(v: JsVal): js.Any = v.value

  implicit def jsVal2String(v: JsVal): js.Any = v.toString
  def parse(value: String) = new JsVal(js.JSON.parse(value))
  def apply(value: js.Any) = new JsVal(value.asInstanceOf[js.Dynamic])
  def obj(keyValues: (String, js.Any)*) = {
    val obj = new js.Object().asInstanceOf[js.Dynamic]
    for ((k, v) <- keyValues){
      obj.updateDynamic(k)(v.asInstanceOf[js.Any])
    }
    new JsVal(obj)
  }
  def arr(values: js.Any*) = {
    new JsVal((values.toArray[js.Any]: js.Array[js.Any]).asInstanceOf[js.Dynamic])
  }
}
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



  class Alpine(authToken: String, host:String, port: Int) {
    val rootUrl = s"http://$host:$port"
    val baseApiUrl = s"${rootUrl}/alpinedatalabs/api"
    val jsonApiUrl = s"${rootUrl}/alpinedatalabs/api/v1/json"

    //sync mode
    val baseWorkflowUrl = s"${jsonApiUrl}/workflows"
    val baseProcessUrl = s"${jsonApiUrl}/processes"

    def getRunUrl(workflowId: String) =
      s"$baseWorkflowUrl/$workflowId/run?token=$authToken&showProgress=true"

    def getQueryUrl(processId: String) =
      s"$baseProcessUrl/$processId/query?token=$authToken"

  }

  object Alpine {
    def apply(authToken: String, host:String, port: Int) = new Alpine(authToken, host, port)
  }


  case class Flow (flowId: String , name: String, `type`: String) {
    val ops = mutable.ListBuffer.empty[Operator]
    def addOperator(op: Operator) {
      ops += op
    }

    def show() {
      ops.foreach(_.draw())
      ops.foreach(_.drawLinks())
    }

    def run() {
      val workflowId = "65"
      val alpine: Alpine = Alpine("1148636c8c5c53ec3ab865b5c05571e0809a4fe3", "localhost", 9090)

      val url = alpine.getRunUrl(workflowId)
      val dsl = s"Workflow Run $url"
      //  val processId=""
      //  val url = alpine.getQueryUrl(processId)
      //  val dsl = s"Workflow Query $url"
      Client2.evalDslWithSrc(dsl)
    }

    def run1() {
      val cmd = """RunDSL get data from SparkSQL at "local[4]" using features "name","age" from table "people" limit 10"""
      Client2.evalDslWithSrc(cmd)
    }
  }
}

@JSExport
object ScalaJSExample {

  import Draw._
  var startPoint = Point(dom.innerWidth.toInt/2 -500 , dom.innerHeight.toInt/2 -50 )

  def draw() = {
    renderer.clearRect(0, 0, canvas.width, canvas.height)
    val op1 = Operator("data",startPoint)
    val op2 = Operator("row filter",Point(startPoint.x +100, startPoint.y -50 ))
    op1.link(op2)

    val f = Flow("16", "test", "cdh4")
    f.addOperator(op1)
    f.addOperator(op2)
    f.show()

    var current = op2
    for ( i <- 0 until 5 ) {
      val op = Operator(s"row filter_$i", Point(current.pos.x + 10, current.pos.y -30 ))
      current.link(op)
      f.addOperator(op)
      current = op
    }

//    f.show()
//    f.run()

  }

  @JSExport
  def main(args: Array[String] ): Unit = {
    draw()
  }
}