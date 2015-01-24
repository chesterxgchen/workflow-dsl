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
 * User: chester
 * Date: 4/23/14
 * Time: 3:42 PM
 */


class Alpine(authToken: String, host:String, port: Int) {
  val rootUrl = s"http://$host:$port"
  val baseApiUrl = s"${rootUrl}/alpinedatalabs/api"
  val jsonApiUrl = s"${rootUrl}/alpinedatalabs/api/v1/json"

  //sync mode
  val baseWorkflowUrl = s"${jsonApiUrl}/sync/workflows"

/*
  def run(workflowId: String) = async {
    val apiUrl = s"$baseWorkflowUrl/$workflowId/run?token=$authToken&showProgress=true&callback=?"
    val res = await(Ajax.post(apiUrl))
    res.onprogress = { p : ProgressEvent =>
      println(res.responseText)
    }
    //   val result = JsVal.parse(res.responseText)
  }
*/

  def getUrl(workflowId: String) =
    s"$baseWorkflowUrl/$workflowId/run?token=$authToken&showProgress=true"

}

object Alpine {
  def apply(authToken: String, host:String, port: Int) = new Alpine(authToken, host, port)
}

