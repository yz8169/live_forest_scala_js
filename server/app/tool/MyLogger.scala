package tool

import javax.inject.Inject
import org.slf4j.MarkerFactory
import play.api.mvc.RequestHeader
import play.api.{Logger, MarkerContext}

/**
  * Created by yz on 2019/5/7
  */
 case class MyLogger @Inject()(tool: Tool)(clazz: Class[_])  {

  val logger = Logger(clazz)

  val marker = MarkerFactory.getMarker("MyMarker")
  implicit val mc: MarkerContext = MarkerContext(marker)

  def debug(message:Any)(implicit request: RequestHeader) = {
    if(tool.isTestMode){
      logger.debug(s"${request.path}  ${message}")
    }

  }

}
