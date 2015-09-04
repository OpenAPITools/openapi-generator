package io.swagger.client.api

import io.swagger.client.model.Order
import io.swagger.client._
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import collection.mutable

class StoreApi(client: TransportClient, config: SwaggerConfig) extends ApiClient(client, config) {

  
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

  
  def placeOrder(body: Option[Order] = None
      )(implicit reader: ClientResponseReader[Order], writer: RequestWriter[Order]): Future[Order] = {
    // create path and map variables
    val path = (addFmt("/store/order"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("POST", path, queryParams.toMap, headerParams.toMap, writer.write(body))
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

    

    

    

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def deleteOrder(orderId: String)(implicit reader: ClientResponseReader[Unit]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/store/order/{orderId}")
        replaceAll ("\\{" + "orderId" + "\\}",orderId.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("DELETE", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  

}
