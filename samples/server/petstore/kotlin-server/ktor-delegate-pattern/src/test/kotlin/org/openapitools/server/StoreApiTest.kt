package org.openapitools.server

import io.ktor.client.call.body
import io.ktor.client.request.*
import io.ktor.http.ContentType
import io.ktor.http.HttpStatusCode
import io.ktor.http.contentType
import io.ktor.server.routing.RoutingCall
import io.ktor.server.plugins.di.dependencies
import org.openapitools.server.apis.StoreApiDelegate
import org.openapitools.server.models.Order
import kotlin.test.Test
import kotlin.test.assertNull
import kotlin.test.assertEquals

class StoreApiTest {

    @Test
    fun testPlaceOrderShouldAddOrder() = petstoreTestApplication {
        var receivedOrder: Order? = null
        class StoreApiImpl: StoreApiDelegate {
            override suspend fun placeOrder(order: Order, call: RoutingCall): Order {
                receivedOrder = order
                return order.copy()
            }
        }
        application.dependencies.provide<StoreApiDelegate> { StoreApiImpl() }
        val order = Order(id = 1, petId = 1, quantity = 1, status = Order.Status.placed)
        val response = client.post("/store/order") {
            contentType(ContentType.Application.Json)
            setBody(order)
        }
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(order, receivedOrder)
        assertEquals(order, response.body<Order>())
    }

    @Test
    fun testPlaceOrderShouldRejectInvalidStatus() = petstoreTestApplication {
        var receivedOrder: Order? = null
        class StoreApiImpl: StoreApiDelegate {
            override suspend fun placeOrder(order: Order, call: RoutingCall): Order {
                receivedOrder = order
                return order.copy()
            }
        }
        application.dependencies.provide<StoreApiDelegate> { StoreApiImpl() }

        val response = client.post("/store/order") {
            contentType(ContentType.Application.Json)
            setBody("""{"id":1, "petId":1, "quantity":1, "status":"invalid"}""")
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedOrder)
    }

    @Test
    fun testPlaceOrderShouldRejectInvalidQuantityFormat() = petstoreTestApplication {
        var receivedOrder: Order? = null
        class StoreApiImpl: StoreApiDelegate {
            override suspend fun placeOrder(order: Order, call: RoutingCall): Order {
                receivedOrder = order
                return order.copy()
            }
        }
        application.dependencies.provide<StoreApiDelegate> { StoreApiImpl() }

        val response = client.post("/store/order") {
            contentType(ContentType.Application.Json)
            setBody("""{"id":1, "petId":1, "quantity":"not-an-int", "status":"placed"}""")
        }
        assertEquals(HttpStatusCode.BadRequest, response.status)
        assertNull(receivedOrder)
    }

    @Test
    fun testGetOrderByIdShouldValidateMin() = petstoreTestApplication {
        class StoreApiImpl: StoreApiDelegate
        application.dependencies.provide<StoreApiDelegate> { StoreApiImpl() }

        val response = client.get("/store/order/0")
        assertEquals(HttpStatusCode.BadRequest, response.status)
    }

    @Test
    fun testGetOrderByIdShouldValidateMax() = petstoreTestApplication {
        class StoreApiImpl: StoreApiDelegate
        application.dependencies.provide<StoreApiDelegate> { StoreApiImpl() }

        val response = client.get("/store/order/6")
        assertEquals(HttpStatusCode.BadRequest, response.status)
    }

    @Test
    fun testGetOrderByIdShouldValidateSuccess() = petstoreTestApplication {
        val order = Order(id = 3, petId = 1, quantity = 1, status = Order.Status.placed)
        class StoreApiImpl: StoreApiDelegate {
            override suspend fun getOrderById(orderId: Long, call: RoutingCall): Order {
                return order
            }
        }
        application.dependencies.provide<StoreApiDelegate> { StoreApiImpl() }

        val response = client.get("/store/order/3")
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(order, response.body<Order>())
    }

    @Test
    fun testGetInventoryShouldReturnInventory() = petstoreTestApplication {
        val inventory = mapOf("available" to 10)
        class StoreApiImpl: StoreApiDelegate {
            override suspend fun getInventory(call: RoutingCall): Map<String, Int> {
                return inventory
            }
        }
        application.dependencies.provide<StoreApiDelegate> { StoreApiImpl() }

        val response = client.get("/store/inventory")
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals(inventory, response.body<Map<String, Int>>())
    }

    @Test
    fun testDeleteOrderShouldDeleteOrder() = petstoreTestApplication {
        var receivedOrderId: String? = null
        class StoreApiImpl: StoreApiDelegate {
            override suspend fun deleteOrder(orderId: String, call: RoutingCall) {
                receivedOrderId = orderId
            }
        }
        application.dependencies.provide<StoreApiDelegate> { StoreApiImpl() }

        val response = client.delete("/store/order/1")
        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals("1", receivedOrderId)
    }
}
