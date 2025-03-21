package org.openapitools.server.api.api

import org.openapitools.server.api.model.Order

import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size

import jakarta.inject.Inject
import jakarta.inject.Singleton

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
        //private val storeApi: StoreApi
    ) : WebAction {

        @Delete("/store/order/{orderId}")
        @Description("Delete purchase order by ID")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun deleteOrder( @PathParam("orderId") orderId: kotlin.String) {
            TODO()
        }

        @Get("/store/inventory")
        @Description("Returns pet inventories by status")
        @ResponseContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun getInventory() {
            TODO()
        }

        @Get("/store/order/{orderId}")
        @Description("Find purchase order by ID")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun getOrderById(@Min(1L) @Max(5L)  @PathParam("orderId") orderId: kotlin.Long) {
            TODO()
        }

        @Post("/store/order")
        @Description("Place an order for a pet")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun placeOrder(@Valid @RequestBody order: Order) {
            TODO()
        }
    }
