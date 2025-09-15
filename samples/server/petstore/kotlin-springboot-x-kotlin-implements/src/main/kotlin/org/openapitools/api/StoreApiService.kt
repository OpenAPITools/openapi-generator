package org.openapitools.api

import org.openapitools.model.Order

interface StoreApiService {

    /**
     * DELETE /store/order/{orderId} : Delete purchase order by ID
     *
     * @param orderId  (required)
     * @return Invalid ID supplied (status code 400)
     *         or Order not found (status code 404)
     * @see StoreApi#deleteOrder
     */
    fun deleteOrder(orderId: kotlin.String): Unit

    /**
     * GET /store/inventory : Returns pet inventories by status
     *
     * @return successful operation (status code 200)
     * @see StoreApi#getInventory
     */
    fun getInventory(): Map<String, kotlin.Int>

    /**
     * GET /store/order/{orderId} : Find purchase order by ID
     *
     * @param orderId  (required)
     * @return successful operation (status code 200)
     *         or Invalid ID supplied (status code 400)
     *         or Order not found (status code 404)
     * @see StoreApi#getOrderById
     */
    fun getOrderById(orderId: kotlin.Int): Order

    /**
     * POST /store/order : Place an order for a pet
     *
     * @param order  (required)
     * @return successful operation (status code 200)
     *         or Invalid Order (status code 400)
     * @see StoreApi#placeOrder
     */
    fun placeOrder(order: Order): Order
}
