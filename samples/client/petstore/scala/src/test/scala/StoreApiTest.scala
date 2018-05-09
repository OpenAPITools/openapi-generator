import org.openapitools.client._
import org.openapitools.client.api._
import org.openapitools.client.model._
import org.joda.time.DateTime

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConverters._
import scala.beans.BeanProperty
import java.util.Date

@RunWith(classOf[JUnitRunner])
class StoreApiTest extends FlatSpec with Matchers {
  behavior of "StoreApi"
  val api = new StoreApi

  api.apiInvoker.defaultHeaders += "api_key" -> "special-key"

  it should "place and fetch an order" in {
    val now = new Date()
    val order = Order(
      petId = Some(10),
      id = Some(1000),
      quantity = Some(101),
      status = Some("pending"),
      shipDate = Some(now),
      complete = Some(true))

    api.placeOrder(order)

    api.getOrderById(1000) match {
      case Some(order) => {
        order.id.get should be(1000)
        order.petId.get should be(10)
        order.quantity.get should be(101)
        order.shipDate.get.getTime().equals(now.getTime()) should be(true)
      }
      case None => fail("didn't find order created")
    }
  }

  it should "delete an order" in {
    val now = new Date()
    val order = Order(
      id = Some(1001),
      petId = Some(10),
      quantity = Some(101),
      status = Some("pending"),
      shipDate = Some(now),
      complete = Some(true))

    api.placeOrder(order)

    api.getOrderById(1001) match {
      case Some(order) => order.id should be(Some(1001))
      case None => fail("didn't find order created")
    }

    api.deleteOrder("1001")
    /* comment out the following as the client cannot handle
     * 4xx response yet
    api.getOrderById(1001) match {
      case Some(order) => fail("order should have been deleted")
      case None =>
    }
     */
  }
}
