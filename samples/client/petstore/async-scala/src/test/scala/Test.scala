import java.net.URI

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}

@RunWith(classOf[JUnitRunner])
class SimpleTest extends FlatSpec with Matchers {
  implicit val formats = org.json4s.DefaultFormats
  implicit val reader = JsonFormatsReader
  implicit val writer = JsonFormatsWriter

  it should "call the api" in {
    val config = SwaggerConfig.forUrl(new URI("http://petstore.swagger.io/v2"))
    val client = new SwaggerClient(config)

    val future = client.pet.getPetById(3)
    val await = Await.ready(future, Duration.Inf)

    await onComplete {
      case Success(pet) => {
        println(pet)
      }
      case Failure(t) => println("failed " + t.getMessage)
    }
  }
}