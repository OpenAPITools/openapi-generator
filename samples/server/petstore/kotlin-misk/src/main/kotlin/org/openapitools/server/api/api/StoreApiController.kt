package org.openapitools.server.api.api

import org.openapitools.server.api.model.Order


import jakarta.inject.Inject
import jakarta.inject.Singleton

// TODO("Only import what we need")
import misk.web.Delete
import misk.web.Description
import misk.web.Get
import misk.web.HttpCall
import misk.web.PathParam
import misk.web.Post
import misk.web.Put
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestContentType
import misk.web.RequestHeaders
import misk.web.ResponseContentType
import misk.web.actions.WebAction
import misk.web.interceptors.LogRequestResponse
import misk.web.mediatype.MediaTypes
import okhttp3.Headers

    @Singleton
    class StoreApiController @Inject constructor(
        // @TODO("camelCase this")
        private val  : StoreApi
    ) : StoreApi, WebAction {

        @Delete("/store/order/{orderId}")
        @Description("Delete purchase order by ID")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun deleteOrder(@PathParam("orderId") orderId: kotlin.String) =
            .deleteOrder(orderId)

        @Get("/store/inventory")
        @Description("Returns pet inventories by status")
        @ResponseContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun getInventory() =
            .getInventory()

        @Get("/store/order/{orderId}")
        @Description("Find purchase order by ID")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun getOrderById(@PathParam("orderId") orderId: kotlin.Long) =
            .getOrderById(orderId)

        @Post("/store/order")
        @Description("Place an order for a pet")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun placeOrder(@RequestBody(required = false) order: Order) =
            .placeOrder(order)
    }
