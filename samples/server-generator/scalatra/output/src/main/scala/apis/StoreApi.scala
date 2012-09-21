package apis

import com.wordnik.client.model.Order
import com.wordnik.swagger.core.ApiPropertiesReader
import org.scalatra.{ TypedParamSupport, ScalatraServlet }
import org.scalatra.swagger._
import org.scalatra.json._

import scala.collection.JavaConverters._
import org.json4s.{ DefaultFormats, Formats }

import scala.collection.JavaConverters._

class StoreApi (implicit val swagger: Swagger) extends ScalatraServlet with TypedParamSupport with JacksonJsonSupport with JValueResult with SwaggerSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  protected val applicationDescription: String = "StoreApi"

  def swaggerToModel(cls: Class[_]) = {
    val docObj = ApiPropertiesReader.read(cls)
    val name = docObj.getName
    val fields = for (field <- docObj.getFields.asScala.filter(d => d.paramType != null))
      yield (field.name -> ModelField(field.name, field.notes, DataType(field.paramType)))

    Model(name, name, fields.toMap)
  }

  before() {
    contentType = formats("json")
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }

  get("/order/:orderId",
    summary("Find purchase order by ID"),
    nickname("getOrderById"),
    responseClass("Order"),
    endpoint("order/{orderId}"),
    notes("For valid response try integer IDs with value <= 5. Anything above 5 or nonintegers will generate API errors"),
    parameters(
      Parameter("orderId", "ID of pet that needs to be fetched",
        dataType = DataType.String,
        paramType = ParamType.Path)
      
      )) {
  }

  delete("/order/:orderId",
    summary("Delete purchase order by ID"),
    nickname("deleteOrder"),
    responseClass("void"),
    endpoint("order/{orderId}"),
    notes("For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors"),
    parameters(
      Parameter("orderId", "ID of the order that needs to be deleted",
        dataType = DataType.String,
        paramType = ParamType.Path)
      
      )) {
  }

  post("/order",
    summary("Place an order for a pet"),
    nickname("placeOrder"),
    responseClass("void"),
    endpoint("order"),
    notes(""),
    parameters(
      Parameter("body", "order placed for purchasing the pet",
        dataType = DataType("Order"),
        paramType = ParamType.Body)
      
      )) {
  }

  }
