package apis

import com.wordnik.client.model.Order
import java.io.File

import org.scalatra.{ TypedParamSupport, ScalatraServlet }
import org.scalatra.swagger._
import org.json4s._
import org.json4s.JsonDSL._
import org.scalatra.json.{ JValueResult, JacksonJsonSupport }
import org.scalatra.servlet.{ FileUploadSupport, MultipartConfig, SizeConstraintExceededException }

import scala.collection.JavaConverters._

class StoreApi(implicit val swagger: Swagger) extends ScalatraServlet
    with FileUploadSupport
    with JacksonJsonSupport
    with SwaggerSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  protected val applicationDescription: String = "StoreApi"
  override protected val applicationName: Option[String] = Some("store")

  before() {
    contentType = formats("json")
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }

  val getOrderByIdOperation = (apiOperation[Order]("getOrderById")
    summary "Find purchase order by ID"
    parameters (
      pathParam[String]("orderId").description(""))
  )

  get("/order/:orderId", operation(getOrderByIdOperation)) {
    val orderId = params.getOrElse("orderId", halt(400))
    println("orderId: " + orderId)
  }

  val deleteOrderOperation = (apiOperation[Unit]("deleteOrder")
    summary "Delete purchase order by ID"
    parameters (
      pathParam[String]("orderId").description(""))
  )

  delete("/order/:orderId", operation(deleteOrderOperation)) {
    val orderId = params.getOrElse("orderId", halt(400))
    println("orderId: " + orderId)
  }

  val placeOrderOperation = (apiOperation[Unit]("placeOrder")
    summary "Place an order for a pet"
    parameters (
      bodyParam[Order]("body").description(""))
  )

  post("/order", operation(placeOrderOperation)) {
    val body = parsedBody.extract[Order]
    println("body: " + body)
  }

}
