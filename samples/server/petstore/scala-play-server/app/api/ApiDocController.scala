package api

import javax.inject.{Inject, Singleton}
import play.api.mvc._

@Singleton
class ApiDocController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
  def api: Action[AnyContent] = Action {
    Redirect("/assets/lib/swagger-ui/index.html?url=/assets/openapi.json")
  }
}
