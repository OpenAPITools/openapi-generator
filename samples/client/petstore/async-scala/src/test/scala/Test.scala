import io.swagger.client.SwaggerClient
import io.swagger.client.api._
import com.wordnik.swagger.client._
import com.wordnik.swagger.client.ClientResponseReaders.Json4sFormatsReader._
import com.wordnik.swagger.client.RequestWriters.Json4sFormatsWriter._

import org.json4s.Formats

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global
import java.net.URI

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

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