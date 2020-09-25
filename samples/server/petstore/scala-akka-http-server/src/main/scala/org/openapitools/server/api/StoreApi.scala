package org.openapitools.server.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.http.scaladsl.unmarshalling.FromStringUnmarshaller
import org.openapitools.server.AkkaHttpHelper._
import org.openapitools.server.model.Order


class StoreApi(
    storeService: StoreApiService,
    storeMarshaller: StoreApiMarshaller
) {

  
  import storeMarshaller._

  lazy val route: Route =
    path("store" / "order" / Segment) { (orderId) => 
      delete {  
            storeService.deleteOrder(orderId = orderId)
      }
    } ~
    path("store" / "inventory") { 
      get {  
            storeService.getInventory()
      }
    } ~
    path("store" / "order" / LongNumber) { (orderId) => 
      get {  
            storeService.getOrderById(orderId = orderId)
      }
    } ~
    path("store" / "order") { 
      post {  
            entity(as[Order]){ order =>
              storeService.placeOrder(order = order)
            }
      }
    }
}


trait StoreApiService {

  def deleteOrder400: Route =
    complete((400, "Invalid ID supplied"))
  def deleteOrder404: Route =
    complete((404, "Order not found"))
  /**
   * Code: 400, Message: Invalid ID supplied
   * Code: 404, Message: Order not found
   */
  def deleteOrder(orderId: String): Route

  def getInventory200(responseMapmap: Map[String, Int])(implicit toEntityMarshallerMapmap: ToEntityMarshaller[Map[String, Int]]): Route =
    complete((200, responseMapmap))
  /**
   * Code: 200, Message: successful operation, DataType: Map[String, Int]
   */
  def getInventory(): Route

  def getOrderById200(responseOrder: Order)(implicit toEntityMarshallerOrder: ToEntityMarshaller[Order]): Route =
    complete((200, responseOrder))
  def getOrderById400: Route =
    complete((400, "Invalid ID supplied"))
  def getOrderById404: Route =
    complete((404, "Order not found"))
  /**
   * Code: 200, Message: successful operation, DataType: Order
   * Code: 400, Message: Invalid ID supplied
   * Code: 404, Message: Order not found
   */
  def getOrderById(orderId: Long)
      (implicit toEntityMarshallerOrder: ToEntityMarshaller[Order]): Route

  def placeOrder200(responseOrder: Order)(implicit toEntityMarshallerOrder: ToEntityMarshaller[Order]): Route =
    complete((200, responseOrder))
  def placeOrder400: Route =
    complete((400, "Invalid Order"))
  /**
   * Code: 200, Message: successful operation, DataType: Order
   * Code: 400, Message: Invalid Order
   */
  def placeOrder(order: Order)
      (implicit toEntityMarshallerOrder: ToEntityMarshaller[Order]): Route

}

trait StoreApiMarshaller {
  implicit def fromEntityUnmarshallerOrder: FromEntityUnmarshaller[Order]



  implicit def toEntityMarshallerOrder: ToEntityMarshaller[Order]

}

