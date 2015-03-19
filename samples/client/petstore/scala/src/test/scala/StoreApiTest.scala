import io.swagger.client._
import io.swagger.client.api._
import io.swagger.client.model._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConversions._
import scala.beans.BeanProperty

@RunWith(classOf[JUnitRunner])
class StoreApiTest extends FlatSpec with Matchers {
  behavior of "StoreApi"
  val api = new StoreApi

  api.apiInvoker.defaultHeaders += "api_key" -> "special-key"

  it should "fetch an order" in {
    api.getOrderById("1") match {
      case Some(order) => {
        order.id should be(1)
        order.petId should be(1)
        order.quantity should be(2)
        order.shipDate should not be (null)
      }
      case None => fail("didn't find order")
    }
  }

  it should "place an order" in {
    val now = new org.joda.time.DateTime
    val order = Order (
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
        order.shipDate.equals(now) should be (true)
      }
      case None =>
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
      case Some(order) => {
        order.id should be(1001)
        order.petId should be(10)
        order.quantity should be(101)
        order.shipDate.equals(now) should be (true)
      }
      case None =>
    }
  }
}