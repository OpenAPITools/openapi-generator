package org.openapitools.api

import org.openapitools.model.OrderStatus
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.RestController

/**
 * Sample implementation of [OrdersApi] demonstrating that the generated
 * `@ModelAttribute("baseName")` annotations correctly bind deepObject query
 * parameters to their respective controller method parameters at runtime —
 * even when multiple parameters share the same type ([OrderStatus]) and when
 * the wire name is not a valid Kotlin identifier (e.g. `order-status`).
 *
 * Each method receives its parameters already resolved by Spring's
 * `ModelAttributeMethodProcessor` and asserts the expected state.
 * When binding is correct the assertion passes and HTTP 200 is returned.
 * If a parameter was routed to the wrong argument (or not bound at all),
 * the assertion throws [IllegalStateException] and the request fails with
 * HTTP 500, which causes any calling test to fail with a clear message.
 *
 * Sentinel values are chosen to be distinct per wire name so that a
 * mis-routing (e.g. `order-status.statuses` landing in `filter`) is caught
 * by the wrong-value check rather than going unnoticed.
 */
@RestController
class OrdersApiImpl : OrdersApi {

    /**
     * All three params present simultaneously with distinct sentinel values.
     *
     * Verifies that `filter.statuses`, `order-status.statuses` and `item-status.statuses`
     * are each routed to their own parameter despite all three having the same type.
     */
    override fun listOrders(
        filter: OrderStatus?,
        orderStatus: OrderStatus?,
        itemStatus: OrderStatus?,
    ): ResponseEntity<Unit> {
        check(filter?.statuses?.contains("sent-via-filter") == true) {
            "filter.statuses: expected to contain 'sent-via-filter', got '${filter?.statuses}'"
        }
        check(orderStatus?.statuses?.contains("sent-via-order-status") == true) {
            "orderStatus.statuses: expected to contain 'sent-via-order-status', got '${orderStatus?.statuses}'"
        }
        check(itemStatus?.statuses?.contains("sent-via-item-status") == true) {
            "itemStatus.statuses: expected to contain 'sent-via-item-status', got '${itemStatus?.statuses}'"
        }
        return ResponseEntity(HttpStatus.OK)
    }

    /**
     * Only `filter` present — verifies that `filter.statuses` binds to the `filter` param.
     *
     * Two values (`active`, `pending`) are sent to confirm that multi-value `Set<String>`
     * binding works as well as single-value routing.
     */
    override fun listOrdersFilterOnly(filter: OrderStatus?): ResponseEntity<Unit> {
        check(filter?.statuses?.containsAll(setOf("active", "pending")) == true) {
            "filter.statuses: expected to contain both 'active' and 'pending', got '${filter?.statuses}'"
        }
        return ResponseEntity(HttpStatus.OK)
    }

    /**
     * Only `order-status` present — verifies that `order-status.statuses` binds to
     * the `orderStatus` param via `@ModelAttribute("order-status")`.
     *
     * Without the explicit annotation value the hyphenated wire name cannot be
     * matched to any Kotlin identifier and `orderStatus` would remain `null`.
     */
    override fun listOrdersOrderStatusOnly(orderStatus: OrderStatus?): ResponseEntity<Unit> {
        check(orderStatus?.statuses?.contains("sent-via-order-status") == true) {
            "orderStatus.statuses: expected to contain 'sent-via-order-status', got '${orderStatus?.statuses}'"
        }
        return ResponseEntity(HttpStatus.OK)
    }

    /**
     * Only `item-status` present — verifies that `item-status.statuses` binds to
     * the `itemStatus` param via `@ModelAttribute("item-status")`.
     */
    override fun listOrdersItemStatusOnly(itemStatus: OrderStatus?): ResponseEntity<Unit> {
        check(itemStatus?.statuses?.contains("sent-via-item-status") == true) {
            "itemStatus.statuses: expected to contain 'sent-via-item-status', got '${itemStatus?.statuses}'"
        }
        return ResponseEntity(HttpStatus.OK)
    }
}
