package fiddle

import org.scalajs.dom.extensions.Ajax
import scala.async.Async.{async, await}
import org.scalajs.dom.extensions.Ajax
import scala.async.Async.{async, await}
import scala.concurrent.{Promise, Future}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import org.scalajs.dom.ProgressEvent



/**
 *
 * "http://maps.googleapis.com/maps/api/elevation/json?locations=27.988056,86.925278&sensor=false"
 *
 * {
    "results" : [
      {
         "elevation" : 8815.7158203125,
         "location" : {
            "lat" : 27.988056,
            "lng" : 86.92527800000001
         },
         "resolution" : 152.7032318115234
      }
   ],
   "status" : "OK"
}
 *
 * User: chester
 * Date: 4/23/14
 * Time: 3:43 PM
 */
object Elevation {

  def run() = async {
    val url = "http://maps.googleapis.com/maps/api/elevation/json?locations=27.988056,86.925278&sensor=false"

    val res = await(Ajax.post(url,
                    headers =  Seq("`Access-Control-Allow-Origin`" -> "*",
                                   "`Access-Control-Allow-Headers`" -> "Content-Type,Origin, Content-Length",
                                   "Access-Control-Allow-Methods" -> "OPTIONS,GET")))
    res.onprogress =  {p : ProgressEvent =>
      println("progress" + res.responseText )
    }

    val result = JsVal.parse(res.responseText)
    result("results").toString()
  }

}
