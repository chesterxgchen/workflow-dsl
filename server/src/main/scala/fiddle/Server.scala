package fiddle
import acyclic.file
import spray.http._
import spray.http.HttpHeaders._
import spray.httpx.encoding.Gzip
import spray.routing.directives.CachingDirectives._
import scala.Some
import spray.routing.{RequestContext, SimpleRoutingApp}
import akka.actor.ActorSystem
import spray.routing.directives.CacheKeyer
import scala.collection.mutable
import java.security.{AccessControlException, Permission}
import java.io.FilePermission
import java.util.PropertyPermission
import java.lang.reflect.ReflectPermission
import java.net.SocketPermission

import spray.client.pipelining._

import play.api.libs.json._
import scala.concurrent._
import scala.tools.nsc.interpreter.Completion
import scala.reflect.internal.util.OffsetPosition
import spray.http.HttpHeaders.`Cache-Control`
import spray.http.CacheDirectives.{`max-age`, `public`}
import spray.http.HttpRequest
import spray.routing.RequestContext
import scala.Some
import spray.http.HttpResponse
import spray.http.CacheDirectives.`max-age`
import spray.routing._
import scala.util.{Try, Success, Failure}
import akka.event.Logging
import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import spray.json.JsonFormat
import java.lang.String
import scala.Predef._
import scala.async.Async


case class Location(lat: Double, lng: Double)
case class Elevation(location: Location, elevation: Double)
case class GoogleApiResult[T](status: String, results: List[T])

object ElevationJsonProtocol extends DefaultJsonProtocol {
  implicit val locationFormat = jsonFormat2(Location)
  implicit val elevationFormat = jsonFormat2(Elevation)
  implicit def googleApiResultFormat[T :JsonFormat] = jsonFormat2(GoogleApiResult.apply[T])
}

object Server extends SimpleRoutingApp {
  implicit val system = ActorSystem()
  import system.dispatcher

  def main(args: Array[String]): Unit = {
    implicit val Default: CacheKeyer = CacheKeyer {
      case RequestContext(HttpRequest(_, uri, _, entity, _), _, _) => (uri, entity)
    }

    val clientFiles = Seq("/client-opt.js")
    val simpleCache = routeCache(maxCapacity = 1000)
    startServer("localhost", port = 28080) {
      cache(simpleCache) {
        encodeResponse(Gzip) {
          get {
            pathSingleSlash {
              complete{
                HttpEntity(
                  MediaTypes.`text/html`,
                  Static.page(
                    s"Client().gistMain([])",
                    clientFiles,
                    "Loading gist...")
                )
              }
            } ~
            path("gist" / Segments){ i =>
              complete{
                HttpEntity(
                  MediaTypes.`text/html`,
                  Static.page(
                    s"Client().gistMain(${JsArray(i.map(JsString)).toString()})",
                    clientFiles,
                    "Loading gist..."
                  )
                )
              }
            } ~ path("evaluation"){
              formFields("compiled", "source"){ (c,s) =>
                renderCode(c, Seq("/page-opt.js"), s, s"Page().updatePage('$s')", analytics = false)
              }
            } ~
            getFromResourceDirectory("")
          } ~
          post {
            path("compile"){
              compileStuff(_, Compiler.packageUserFiles _ andThen funcWrap)
            } ~
            path("optimize"){
              compileStuff(_, Compiler.optimize _ andThen funcWrap)
            } ~
            path("preoptimize"){
              compileStuff(_, Compiler.deadCodeElimination _ andThen funcWrap)
            } ~
            path("extdeps"){
              complete{
                Compiler.packageJS(Compiler.classPath)
              }
            } ~
            path("export"){
              formFields("compiled", "source"){
                renderCode(_, Seq("/page-opt.js"), _, "Page().exportMain()", analytics = false)
              }
            } ~
            path("import"){
              formFields("compiled", "source"){
                renderCode(_, clientFiles, _, "Client().importMain()", analytics = true)
              }
            } ~ path("complete" / Segment / IntNumber){
              completeStuff
            } ~ path("evalDsl") {
              evalDSL (_, Compiler.packageUserFiles _ andThen funcWrap  )
            } ~ path("evaluation"){
              println("evaluation something in POST" )
              formFields("compiled", "source"){ (c,s) =>
                renderCode(c, Seq("/page-opt.js"), s, s"Page().println('$s')", analytics = false)
              }
            }
          }
        }
      }
    }
  }
  def renderCode(compiled: String, srcFiles: Seq[String], source: String, bootFunc: String, analytics: Boolean) = {
    val pageContent = Static.page(bootFunc, srcFiles, source, compiled, analytics)
    println("rendering " + pageContent)
    complete{
      HttpEntity(
        MediaTypes.`text/html`,
        pageContent
      )
    }
  }

  def completeStuff(flag: String, offset: Int)(ctx: RequestContext): Unit = {
//    setSecurityManager
    for(res <- Compiler.autocomplete(ctx.request.entity.asString, flag, offset, Compiler.validJars)){
      val response = JsArray(
        res.map{case (label, name) => JsArray(Seq(label, name).map(JsString))}
      )
      println(s"got autocomplete: sending $response")
      ctx.responder ! HttpResponse(
        entity=response.toString(),
        headers=List(
          `Access-Control-Allow-Origin`(spray.http.AllOrigins)
        )
      )
    }
  }
  def funcWrap(s: String) = s"(function(){ $s; ScalaJSExample().main(); console.log('running')}).call(window)"

  def compileStuff(ctx: RequestContext, processor: Seq[(String, String)] => String): Unit = {
    val (code, out, success) = compileSource(ctx, ctx.request.entity.data.toByteArray, processor)
    val returned = Json.obj(
      "success" -> success,
      "logspam" -> (if (out.isEmpty) "empty" else out),
      "code" -> code
    )

    println("out = " + out)
    println("success = " + success)
    ctx.responder ! HttpResponse(
      entity = returned.toString,
      headers = List(
        `Access-Control-Allow-Origin`(spray.http.AllOrigins)
      )
    )


  }

  def compileSource(ctx: RequestContext, src: Array[Byte], processor: Seq[(String, String)] => String):
  (String , String , Boolean ) = {

    val output = mutable.Buffer.empty[String]

    val res = Compiler.compile(
      Compiler.prelude.getBytes ++ src,
      Compiler.validJars,
      output.append(_)
    )
    println("output = " + output)

    val code = res match {
      case None => ""
      case Some(files) =>
        processor( files.map(f => f.name -> new String(f.toByteArray)))
    }

    (code, output.mkString, res.isDefined)
  }



  def evalDSL(ctx: RequestContext, processor: Seq[(String, String)] => String): Unit = {

    var src: Array[Byte] = ctx.request.entity.data.toByteArray
    val (code, logmsg, success) = compileSource(ctx, Array(), processor)

    println("out after compile = " + logmsg)


    val output = mutable.Buffer.empty[String]
    val logout = mutable.Buffer.empty[String]
    logout.append(logmsg)
    logout.append("\n")

     processDsl(src,output, logout)

    val value = output.mkString

    println("output after process dsl = " + value)
    val returned = Json.obj(
      "success" -> success,
      "logspam" -> logout.mkString,
      "code" -> code,
      "output" -> value
    )

    ctx.responder ! HttpResponse(
      entity = returned.toString,
      headers = List(
        `Access-Control-Allow-Origin`(spray.http.AllOrigins)
      )
    )
  }



  def processDsl(src: Array[Byte], output: mutable.Buffer[String],logout: mutable.Buffer[String] ) : Try[Unit] = {

    val log = Logging(system, getClass)

    def makeDSL(src: Array[Byte]) = {
      new String(src)
    }

    def processElevation():Unit = {
      var message1: String = s"Requesting the elevation of Mt. Everest from Googles Elevation API..."
      log.info(message1)
      logout.append(message1)
      logout.append("\n")

      val url = "http://maps.googleapis.com/maps/api/elevation/json?locations=27.988056,86.925278&sensor=false"

      logout.append(url)
      logout.append("\n")

      import ElevationJsonProtocol._
      import SprayJsonSupport._
      val pipeline = sendReceive ~> unmarshal[GoogleApiResult[Elevation]]
      val responseFuture = pipeline {
        Get(url)
      }

      import scala.concurrent.Await
      import scala.concurrent.duration._
      import scala.language.postfixOps._

       try {
         Await.result(responseFuture, 5 seconds) match {
           case GoogleApiResult(_, Elevation(_, elevation) :: _) =>
             val resultMsg = s"The elevation of Mt. Everest is: ${elevation} m"
             log.info(resultMsg)
             output.append(resultMsg.toString)
           case somethingUnexpected: Any =>
             val message = s"The Google API call was successful but returned something unexpected: '$somethingUnexpected'."
             log.warning(message)
             logout.append(message)
             logout.append("\n")
         }
         println("output.mkString = " + output.mkString)
       }
       catch {
         case e: Exception => logout.append(s"failed to get elevation: ${e.getMessage}" )
       }

    }

    def runWorkflow(url: String) = {
/*
      val baseUrl = "http://localhost:9090"
      val workflowId="65"
      val token="1148636c8c5c53ec3ab865b5c05571e0809a4fe3"
      val url = "http://localhost:9090/alpinedatalabs/api/v1/json/workflows/65/run?token=1148636c8c5c53ec3ab865b5c05571e0809a4fe3"
      val url = "http://localhost:9090/alpinedatalabs/api/v1/json/processes/10faff1e-d6eb-47e9-b70f-3cea6949a97c/query?token=1148636c8c5c53ec3ab865b5c05571e0809a4fe3"
*/
     /*
      val p = new ExternalDSL
      val parsedResult = p.parse(p.source, command)


      val p.Success(backend, qryText) = parsedResult
      
      println("apiUrl = " + apiUrl)
      
      logout.append(s"sending get request to url \n ${apiUrl}")
*/
      logout.append(s"trying to post  url '$url'\n")
      try {
        import SprayJsonSupport._
        val pipeline = sendReceive ~> unmarshal[String]
        val responseFuture = pipeline { Post(url.trim) }

        logout.append(s"url posted \n")
        import scala.concurrent.Await
        import scala.concurrent.duration._
        import scala.language.postfixOps._


        logout.append(s"waiting for the post request to return \n")
        val r = Await.result(responseFuture, 15 seconds)
        logout.append(s"get something $r \n")
        output.append(r)

        println("output.mkString = " + output.mkString)

      }
      catch {
        case e: Exception => logout.append(s"failed to run workflow: ${e.getMessage}" )
      }


    }



    def queryWorkflow(url: String) {
      logout.append(s"trying to query workflow with url = '$url'")
      try {
        import SprayJsonSupport._
        val pipeline = sendReceive ~> unmarshal[String]
        val responseFuture = pipeline { Get(url.trim) }
        import scala.concurrent.Await
        import scala.concurrent.duration._
        import scala.language.postfixOps._
        val r = Await.result(responseFuture, 5 seconds)
        output.append(r)

        println("output.mkString = " + output.mkString)
      }
      catch {
        case e: Exception => logout.append(s"failed to query workflow: ${e.getMessage}" )
      }


    }

    val dsl = makeDSL(src)
    println("dsl = " + dsl)
    //do use regex with pattern match or DSL to get rid of if-else
    if (dsl.trim.startsWith("Say")) {
      val dslStr = extractSubString(dsl, "Say")
      output.append(dslStr)
      Success(output.mkString)
    }
    else if (dsl.trim.startsWith("Elevation")) {
      processElevation()
      println("after evaluation output = " + output.mkString)
      Success(output.mkString)
    }
    else if (dsl.trim.startsWith("RunDSL")) {
      val dslStr = extractSubString(dsl, "RunDSL")
      println("dslStr = " + dslStr)
/*
      val p = new ExternalDSL
      val srcResult = p.parse(p.source, dslStr)
      val p.Success(backend, qryText) = srcResult
      val p.Success(qry, _) = p.parse(p.query, qryText)

      val f = Future {
        backend.asInstanceOf[Queryable].execute(qry.toString)
      }
      Try {
        f onSuccess {case s => output.append(s)}
        f onFailure {case t => logout.append(s"${t.getMessage}\n") }
      }
*/

      logout.append(" trying to run DSL\n")
      import scala.sys.process._
      val returnVal = s"java -jar /Users/chester/Downloads/AlpineDSL-assembly-1.0.jar $dslStr".!!
      println("return value = " + returnVal)
      logout.append(s" I got something $returnVal \n")
      Try(output.append(returnVal))

    }
    else if (dsl.trim.startsWith("Workflow")) {
      val dslStr: String = extractSubString(dsl, "Workflow")
      if ( dslStr.startsWith("Run")) {
        val url= extractSubString(dsl, "Run")
        Try(runWorkflow(url))
      }
      else if ( dslStr.startsWith("Query")) {
        val url= extractSubString(dsl, "Query")
        Try(queryWorkflow(url))
      }
      else
        Failure(new RuntimeException("unhandled workflow DSL"))
    }
    else
      Failure(new RuntimeException("unhandled dsl"))

  }


  def extractSubString(dsl: String, key :String ) : String = {
    val index: Int = dsl.indexOf(key)
    dsl.drop(index + key.length).trim
  }
}
