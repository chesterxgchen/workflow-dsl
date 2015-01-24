package fiddle
import acyclic.file
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => lit, _}
import org.scalajs.dom
import scala.concurrent.{Promise, Future}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scala.async.Async.{async, await}
import scalatags.all._
import scalatags._
import rx._
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.extensions.Ajax
import Page.fiddleUrl
import scala.Some
import JsVal.jsVal2jsAny

import scala.Some
import scala.util.{Success, Failure}

class Client(){
  import Page.{log, logln, red, blue, green}

  val command = Channel[(String, String)]()

  def exec(s: String) = {
    Client.clear()
    js.eval(s)
  }

  var compileEndpoint = "/preoptimize"
  var extdeps = ""

  lazy val extdepsLoop = task*async{
    extdeps = await(Ajax.post("/extdeps")).responseText
    compileEndpoint = "/compile"
  }

  val compilationLoop = task*async{
    val (code, cmd) = await(command())
    await(compile(code, cmd)).foreach(exec)

    while(true){
      val (code, cmd) = await(command())
      val compiled = await(compile(code, cmd))

      js.eval(extdeps)
      extdeps = ""
      compiled.foreach(exec)
      extdepsLoop
    }
  }

  val editor: Editor = new Editor(Seq(
    ("Compile", "Enter", () => command.update((editor.code, compileEndpoint))),
    ("PreOptimize", "Alt-Enter", () => command.update((editor.code, "/preoptimize"))),
    ("Optimize", "Shift-Enter", () => command.update((editor.code, "/optimize"))),
    ("Save", "S", save),
    ("Complete", "Space", () => editor.complete()),
    ("Javascript", "J", () => viewJavascript("/compile")),
    ("PreOptimizedJavascript", "Alt-J", () => viewJavascript("/preoptimize")),
    ("OptimizedJavascript", "Shift-J", () => viewJavascript("/optimize")),
    ("Export", "E", export) ,
 //   ("evalDSL", "R", () => command.update((editor.code, "/evalDsl")))
    ("evalDSL", "R", () => evalDsl())
  ), complete)

  logln("- ", blue("Cmd/Ctrl-Enter"), " to compile & execute, ", blue("Cmd/Ctrl-Space"), " for autocomplete.")

  def compile(code: String, endpoint: String): Future[Option[String]] = async {
    if (code == "") None
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
      logln()
      result.get("code").map(_.asString)
    }
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
      logln()
      (result.get("code").map(_.asString), result.get("output").map(_.asString))
    }
  }
  def viewJavascript(endpoint: String) = task*async {
    await(compile(editor.code, endpoint)).foreach{ compiled  =>
      Client.clear()
      Page.output.innerHTML = Page.highlight(compiled , "ace/mode/javascript")
    }
  }

  def complete() = async {
    log("Completing... ")

    val code = editor.sess.getValue().asInstanceOf[String]

    val intOffset = editor.column + code.split("\n")
                                        .take(editor.row)
                                        .map(_.length + 1)
                                        .sum

    val flag = if(code.take(intOffset).endsWith(".")) "member" else "scope"


    val xhr = await(Ajax.post(s"/complete/$flag/$intOffset", code))
    log("Done")
    logln()
    js.JSON
      .parse(xhr.responseText)
      .asInstanceOf[js.Array[js.Array[String]]]
      .toSeq
      .map(_.toSeq)
  }

  def export(): Unit = task*async {
    logln("Exporting...")
    await(compile(editor.code, "/optimize")).foreach{ code =>
      Util.Form.post("/export",
        "source" -> editor.code,
        "compiled" -> code
      )
    }
  }


  def evalDsl(): Unit = task*async {
//    logln("Evaluating DSL...")
//    val (_, output) = await(compile2(editor.code, "/evalDsl"))
//    output.foreach( Page.println(_))
    evalDslWithSrc(editor.code)
  }


  def evalDslWithSrc(src: String ): Unit = task*async {
    logln("Evaluating DSL...")
    val (_, output) = await(compile2(src, "/evalDsl"))
    output.foreach( Page.println(_))
  }


  def save(): Unit = task*async{
    await(compile(editor.code, "/optimize"))
    val data = JsVal.obj(
      "description" -> "Scala.jsFiddle gist",
      "public" -> true,
      "files" -> JsVal.obj(
        "Main.scala" -> JsVal.obj(
          "content" -> editor.code
        )
      )
    ).toString()

    val res = await(Ajax.post("https://api.github.com/gists", data = data))
    val result = JsVal.parse(res.responseText)
    Util.Form.get("/gist/" + result("id").asString)
  }
}

@JSExport
object Client{

  import Page.{canvas, sandbox, logln, red, blue, green}

  def clear() = {
    for(i <- 0 until 10000){
      dom.clearInterval(i)
      dom.clearTimeout(i)
    }
    Page.clear()
    canvas.height = sandbox.clientHeight
    canvas.width = sandbox.clientWidth
  }

  @JSExport
  def gistMain(args: js.Array[String]): Unit = task*async{

    Editor.initEditor
    //    val (gistId, fileName) = args.toSeq match{
    //      case Nil => ("9759723", Some("LandingPage.scala"))
    //      case Seq(g) => (g, None)
    //      case Seq(g, f) => (g, Some(f))
    //    }
    // val src = await(load(gistId, fileName))

   val src  =
     """
        @JSExport
        object ScalaJSExample{
          @JSExport
          def main(args: Array[String]): Unit = {
            println("hello")
          }
        }


       | /*       import scala.scalajs.js
       |        import scala.util.{Success, Failure}
       |        import scala.concurrent.{ExecutionContext, Future, Promise}
       |        import scala.scalajs.js
       |import scala.scalajs.js.Dynamic.{literal => lit, _}
       |import org.scalajs.dom
       |import scala.concurrent.{Promise, Future}
       |import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
       |import scala.async.Async.{async, await}
       |import scalatags.all._
       |import scalatags._
       |import rx._
       |import scala.scalajs.js.annotation.JSExport
       |import org.scalajs.dom.extensions.Ajax
       |
       |@JSExport
       |class JsVal(val value: js.Dynamic){
       |  def get(name: String): Option[JsVal] = {
       |    (value.selectDynamic(name): js.Any) match {
       |      case _: js.Undefined => None
       |      case v => Some(JsVal(v))
       |    }
       |  }
       |  def apply(name: String): JsVal = get(name).get
       |  def apply(index: Int): JsVal = value.asInstanceOf[js.Array[JsVal]](index)
       |
       |  def keys: Seq[String] = js.Object.keys(value.asInstanceOf[js.Object]).toSeq.map(x => x: String)
       |  def values: Seq[JsVal] = keys.toSeq.map(x => JsVal(value.selectDynamic(x)))
       |
       |  def isDefined: Boolean = !(value: js.Any).isInstanceOf[js.Undefined]
       |  def isNull: Boolean = value eq null
       |
       |  def asDouble: Double = value.asInstanceOf[js.Number]
       |  def asBoolean: Boolean = value.asInstanceOf[js.Boolean]
       |  def asString: String = value.asInstanceOf[js.String]
       |
       |  override def toString(): String = js.JSON.stringify(value)
       |}
       |
       |@JSExport
       |object JsVal {
       |  implicit def jsVal2jsAny(v: JsVal): js.Any = v.value
       |
       |  implicit def jsVal2String(v: JsVal): js.Any = v.toString
       |  def parse(value: String) = new JsVal(js.JSON.parse(value))
       |  def apply(value: js.Any) = new JsVal(value.asInstanceOf[js.Dynamic])
       |  def obj(keyValues: (String, js.Any)*) = {
       |    val obj = new js.Object().asInstanceOf[js.Dynamic]
       |    for ((k, v) <- keyValues){
       |      obj.updateDynamic(k)(v.asInstanceOf[js.Any])
       |    }
       |    new JsVal(obj)
       |  }
       |  def arr(values: js.Any*) = {
       |    new JsVal((values.toArray[js.Any]: js.Array[js.Any]).asInstanceOf[js.Dynamic])
       |  }
       |}
       |
       |     @JSExport
       |        object ScalaJSExample {
       |          import scala.concurrent.{Promise, Future}
       |          import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
       |          import JsVal._
       |
       |         def load(): Future[String] = async {
       |             val gistId = "9759723"
       |             val res = await(Ajax.get("https://api.github.com/gists/" + gistId))
       |             val result = JsVal.parse(res.responseText)
       |             val mainFile = result("files").get("BasicOperations.scala")
       |             val firstFile = result("files").values(0)
       |             mainFile.getOrElse(firstFile)("content").asString
       |
       |            // result.toString
       |                }
       |
       |         @JSExport
       |         def main(args: Array[String]): Unit = {
       |           async(
       |                 println(await(load()))
       |              //  println("hello")
       |           )
       |         }
       |
       |
       |
       |      }    */
       |

     """.stripMargin
    val client = new Client()
    client.editor.sess.setValue(src)
    client.command.update((src, "/optimize"))
  }

  @JSExport
  def importMain(): Unit = {
    clear()
    val client = new Client()
    client.command.update(("", "/compile"))
    js.eval(Page.compiled)
  }

  def load(gistId: String, file: Option[String]): Future[String] = async {
    val gistUrl = "https://gist.github.com/" + gistId
    logln(
      "Loading ",
      file.fold(span)(s => span(
        a(href:=gistUrl + "#file-" + s.toLowerCase.replace('.', '-'))(s),
        " from "
      )),
      a(href:=gistUrl)(gistUrl),
      "..."
    )

    val res = await(Ajax.get("https://api.github.com/gists/" + gistId))
    val result = JsVal.parse(res.responseText)
    val mainFile = result("files").get(file.getOrElse(""))
    val firstFile = result("files").values(0)
    mainFile.getOrElse(firstFile)("content").asString
  }
}