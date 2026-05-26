package org.openapitools.api

import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.get

/**
 * Verifies the runtime behaviour of the `@ModelAttribute("baseName")` annotations
 * generated onto [OrdersApi]:
 *
 * - **Valid Kotlin identifier** (`filter`) — `filter.state=X` is bound to the
 *   `filter` parameter, not to any other parameter of the same type.
 * - **Hyphenated wire name** (`order-status`) — `order-status.state=X` is bound
 *   to the `orderStatus` parameter via the explicit `@ModelAttribute("order-status")`
 *   annotation.  Without it the wire name cannot be matched to any Kotlin identifier
 *   and the parameter would remain `null`.
 * - **Second hyphenated wire name** (`item-status`) — same as above for a second
 *   non-identifier name to confirm there is no cross-contamination between params.
 * - **All three simultaneously** — each param receives its own distinct sentinel
 *   value, proving that the three `@ModelAttribute` scopes are independent.
 *
 * HTTP 200 confirms both that the request was accepted *and* that [OrdersApiImpl]'s
 * internal assertions about the bound values passed.
 * HTTP 500 would indicate a failed `check()` inside the impl, meaning a param was
 * either not bound at all or routed to the wrong argument.
 */
@SpringBootTest
@AutoConfigureMockMvc
class OrdersApiValidationTest {

    @Autowired
    lateinit var mockMvc: MockMvc

    // ── filter (valid Kotlin identifier) ─────────────────────────────────────
    // Endpoint: GET /orders/filter-only — impl asserts filter.state == "sent-via-filter"

    @Test
    fun `filter deepObject param binds correctly to filter parameter`() {
        mockMvc.get("${OrdersApi.BASE_PATH}${OrdersApi.PATH_LIST_ORDERS_FILTER_ONLY}") {
            param("filter.statuses", "sent-via-filter")
        }.andExpect { status { isOk() } }
    }

    // ── order-status (non-Kotlin-identifier, hyphenated) ──────────────────────
    // Endpoint: GET /orders/order-status-only — impl asserts orderStatus.state == "sent-via-order-status"

    @Test
    fun `order-status deepObject param binds correctly to orderStatus parameter`() {
        mockMvc.get("${OrdersApi.BASE_PATH}${OrdersApi.PATH_LIST_ORDERS_ORDER_STATUS_ONLY}") {
            param("order-status.statuses", "sent-via-order-status")
        }.andExpect { status { isOk() } }
    }

    // ── item-status (non-Kotlin-identifier, hyphenated) ───────────────────────
    // Endpoint: GET /orders/item-status-only — impl asserts itemStatus.state == "sent-via-item-status"

    @Test
    fun `item-status deepObject param binds correctly to itemStatus parameter`() {
        mockMvc.get("${OrdersApi.BASE_PATH}${OrdersApi.PATH_LIST_ORDERS_ITEM_STATUS_ONLY}") {
            param("item-status.statuses", "sent-via-item-status")
        }.andExpect { status { isOk() } }
    }

    // ── all three simultaneously ──────────────────────────────────────────────
    // Endpoint: GET /orders — impl asserts all three are non-null with their distinct sentinels.
    // If any param were mis-routed (e.g. order-status landing in filter), the wrong-value
    // check in the impl would fire and return HTTP 500.

    @Test
    fun `all three deepObject params bind to their respective parameters`() {
        mockMvc.get("${OrdersApi.BASE_PATH}${OrdersApi.PATH_LIST_ORDERS}") {
            param("filter.statuses", "sent-via-filter")
            param("order-status.statuses", "sent-via-order-status")
            param("item-status.statuses", "sent-via-item-status")
        }.andExpect { status { isOk() } }
    }
}
