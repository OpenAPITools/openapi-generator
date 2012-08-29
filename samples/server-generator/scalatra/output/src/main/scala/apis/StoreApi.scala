package apis

import com.wordnik.swagger.core.ApiPropertiesReader
import com.wordnik.client.model.Order
import org.scalatra.ScalatraServlet
import org.scalatra.swagger._

import scala.collection.JavaConverters._

class StoreApi(implicit val swagger: Swagger) extends ScalatraServlet with SwaggerBase with SwaggerSupport {
  protected def buildFullUrl(path: String) = "http://petstore.swagger.wordnik.com/api/%s" format path

  get("/order/:orderId",
    summary("Find purchase order by ID"),
    nickname("getOrderById"),
    responseClass("Order"),
    endpoint("{TBD}"),
    notes("For valid response try integer IDs with value <= 5. Anything above 5 or nonintegers will generate API errors"),
    parameters(
      Parameter("orderId", "ID of pet that needs to be fetched",
        dataType = DataType.String,
        paramType = ParamType.Path))) {
    }

  delete("/order/:orderId",
    summary("Delete purchase order by ID"),
    nickname("deleteOrder"),
    responseClass("void"),
    endpoint("{TBD}"),
    notes("For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors"),
    parameters(
      Parameter("orderId", "ID of the order that needs to be deleted",
        dataType = DataType.String,
        paramType = ParamType.Path))) {
    }

  post("/order",
    summary("Place an order for a pet"),
    nickname("placeOrder"),
    responseClass("void"),
    endpoint("{TBD}"),
    notes(""),
    parameters(
      Parameter("body", "order placed for purchasing the pet",
        dataType = DataType("Order"),
        paramType = ParamType.Body))) {
    }

}
