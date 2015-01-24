package fiddle

import org.apache.log4j.Logger
import akka.actor.Actor
import akka.event.Logging._
import akka.event.Logging.InitializeLogger


import org.apache.log4j.Logger
import akka.actor.Actor

import akka.event.Logging.InitializeLogger
import akka.event.Logging.LoggerInitialized
import akka.event.Logging.Error
import akka.event.Logging.Warning
import akka.event.Logging.Info
import akka.event.Logging.Debug
/**
 *
 * User: chester
 * Date: 4/25/14
 * Time: 11:53 AM
 */

object ForwardEventListener {
  val logger: Logger = Logger.getLogger("com.alpine.core.akka.event.logging.listener.ForwardEventListener")
}
class ForwardEventListener extends Actor {
  import ForwardEventListener._

  def receive: Actor.Receive = {
    case InitializeLogger(_) => sender ! LoggerInitialized
    case Error(cause, logSource, logClass, message) => logger.error(message, cause)
    case Warning(logSource, logClass, message) => logger.warn(message)
    case Info(logSource, logClass, message) => logger.info(message)
    case Debug(logSource, logClass, message) => logger.debug(message)

  }
}
