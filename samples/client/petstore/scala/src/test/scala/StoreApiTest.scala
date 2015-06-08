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
      petId = 10,
      id = 1000,
      quantity = 101,
      status = "pending",
      shipDate = now,
      complete = true)

    api.placeOrder(order)

    api.getOrderById("1000") match {
      case Some(order) => {
        order.id should be(1000)
        order.petId should be(10)
        order.quantity should be(101)
        // use `getMillis` to compare across timezones
        order.shipDate.getMillis.equals(now.getMillis) should be(true)
      }
      case None => fail("didn't find order created")
    }
  }

  it should "delete an order" in {
    val now = new org.joda.time.DateTime
    val order = Order(
      id = 1001,
      petId = 10,
      quantity = 101,
      status = "pending",
      shipDate = now,
      complete = true)

    api.placeOrder(order)

    api.getOrderById("1001") match {
      case Some(order) => order.id should be(1001)
      case None => fail("didn't find order created")
    }

    api.deleteOrder("1001")
    api.getOrderById("1001") match {
      case Some(order) => fail("order should have been deleted")
      case None =>
    }
  }
}
