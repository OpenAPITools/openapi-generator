import apis._
import com.wordnik.swagger.app.{ResourcesApp, SwaggerApp}
import javax.servlet.ServletContext
import org.scalatra.LifeCycle

class ScalatraBootstrap extends LifeCycle {
  implicit val swagger = new SwaggerApp

  override def init(context: ServletContext) {
    try {
      context mount (new StoreApi, "/store/*")
      context mount (new PetApi, "/pet/*")
      context mount (new UserApi, "/user/*")
      context mount (new ResourcesApp, "/*")
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }
}
