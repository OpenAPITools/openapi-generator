import io.swagger.client._
import io.swagger.client.api._
import io.swagger.client.model._
 
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConverters._
import scala.beans.BeanProperty

@RunWith(classOf[JUnitRunner])
class StoreApiTest extends FlatSpec with Matchers {
  behavior of "StoreApi"
  val api = new StoreApi

  api.apiInvoker.defaultHeaders += "api_key" -> "special-key"

  it should "place and fetch an order" in {
    val now = new org.joda.time.DateTime
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
        order.id should be(Some(1000))
        order.petId should be(Some(10))
        order.quantity should be(Some(101))
        // use `getMillis` to compare across timezones
        order.shipDate.get.getMillis.equals(now.getMillis) should be(true)
      }
      case None => fail("didn't find order created")
    }
  }

  it should "delete an order" in {
    val now = new org.joda.time.DateTime
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
    api.getOrderById(1001) match {
      case Some(order) => fail("order should have been deleted")
      case None =>
    }
  }
}
