package io.swagger.client.api

import io.swagger.client.model.Order
import io.swagger.client.model.Any
import com.wordnik.swagger.client._
import scala.concurrent.Future
import collection.mutable

class StoreApi(client: TransportClient, config: SwaggerConfig) extends ApiClient(client, config) {

  def deleteOrder(orderId: String)(implicit reader: ClientResponseReader[Unit]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/store/order/{orderId}")
        replaceAll ("\\{" + "orderId" + "\\}",orderId.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    // verify required params are set
    val paramCount = (Set[Any]() - null).size
    if (paramCount != ) sys.error("missing required params")

    

    

    val resFuture = client.submit("DELETE", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def findOrdersByStatus(status: Option[String] = Some(placed)
      )(implicit reader: ClientResponseReader[List[Order]]): Future[List[Order]] = {
    // create path and map variables
    val path = (addFmt("/store/findByStatus"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    // verify required params are set
    val paramCount = (Set[Any]() - null).size
    if (paramCount != ) sys.error("missing required params")

    if(status != null) status.foreach { v => queryParams += "status" -> v.toString }

    

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def getInventory()(implicit reader: ClientResponseReader[Map[String, Integer]]): Future[Map[String, Integer]] = {
    // create path and map variables
    val path = (addFmt("/store/inventory"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def getInventoryInObject()(implicit reader: ClientResponseReader[Any]): Future[Any] = {
    // create path and map variables
    val path = (addFmt("/store/inventory?response&#x3D;arbitrary_object"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def getOrderById(orderId: String)(implicit reader: ClientResponseReader[Order]): Future[Order] = {
    // create path and map variables
    val path = (addFmt("/store/order/{orderId}")
        replaceAll ("\\{" + "orderId" + "\\}",orderId.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    // verify required params are set
    val paramCount = (Set[Any]() - null).size
    if (paramCount != ) sys.error("missing required params")

    

    

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def placeOrder(body: Option[Order] = None
      )(implicit reader: ClientResponseReader[Order], writer: RequestWriter[Order]): Future[Order] = {
    // create path and map variables
    val path = (addFmt("/store/order"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    // verify required params are set
    val paramCount = (Set[Any]() - null).size
    if (paramCount != ) sys.error("missing required params")

    

    

    val resFuture = client.submit("POST", path, queryParams.toMap, headerParams.toMap, writer.write(body))
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }


}
