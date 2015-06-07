

class ScalatraBootstrap extends LifeCycle {
  implicit val swagger = new SwaggerApp

  override def init(context: ServletContext) {
    implicit val system = ActorSystem("appActorSystem")
    try {
      context mount(new UserApi, "/User/*")
      context mount(new PetApi, "/Pet/*")
      context mount(new StoreApi, "/Store/*")

      context mount(new ResourcesApp, "/api-docs/*")
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }
}