import com.wordnik.client._
import com.wordnik.petstore.api._
import com.wordnik.petstore.model._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConversions._
import scala.reflect.BeanProperty

@RunWith(classOf[JUnitRunner])
class StoreApiTest extends FlatSpec with ShouldMatchers {
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
    val now = new java.util.Date
    val order = Order (
      1000,
      10,
      "pending",
      101,
      now)

    api.placeOrder(order)

    api.getOrderById("1000") match {
      case Some(order) => {
        order.id should be(1000)
        order.petId should be(10)
        order.quantity should be(101)
        order.shipDate should be (now)
      }
      case None =>
    }
  }

  it should "delete an order" in {
    val now = new java.util.Date
    val order = Order(
      1001,
      10,
      "pending",
      101,
      now)

    api.placeOrder(order)

    api.getOrderById("1001") match {
      case Some(order) => {
        order.id should be(1001)
        order.petId should be(10)
        order.quantity should be(101)
        order.shipDate should be (now)
      }
      case None =>
    }
    
    api.deleteOrder("1001")
    intercept[ApiException] {
      api.getOrderById("1001") should be (None)
    }
  }
}