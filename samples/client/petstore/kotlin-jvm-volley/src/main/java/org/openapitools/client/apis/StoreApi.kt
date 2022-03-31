package org.openapitools.client.apis

import android.content.Context
import com.android.volley.DefaultRetryPolicy
import com.android.volley.Request
import com.android.volley.RequestQueue
import com.android.volley.Response
import com.android.volley.toolbox.BaseHttpStack
import com.android.volley.toolbox.Volley
import java.util.*;
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException
import kotlin.coroutines.suspendCoroutine
import com.google.gson.reflect.TypeToken

import org.openapitools.client.request.IRequestFactory
import org.openapitools.client.request.RequestFactory
import org.openapitools.client.infrastructure.CollectionFormats.*

import org.openapitools.client.models.Order

/*
* If you wish to use a custom http stack with your client you
* can pass that to the request queue like:
* Volley.newRequestQueue(context.applicationContext, myCustomHttpStack)
*/
class StoreApi (
    private val context: Context,
    private val requestQueue: Lazy<RequestQueue> = lazy(initializer = {
        Volley.newRequestQueue(context.applicationContext)
    }),
    private val requestFactory: IRequestFactory = RequestFactory(),
    private val basePath: String = "http://petstore.swagger.io/v2",
    private val postProcessors :List <(Request<*>) -> Unit> = listOf()) {

    /**
    * Delete purchase order by ID
    * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
       * @param orderId ID of the order that needs to be deleted
       * @return void
    */
    suspend fun deleteOrder(orderId: kotlin.String): Unit {
        val body: Any? = null
        // verify the required parameter 'orderId' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(orderId)

        val contentTypes : Array<String> = arrayOf()
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/store/order/{orderId}".replace("{" + "orderId" + "}", IRequestFactory.escapeString(orderId.toString()));

        // form params
        val formParams = mapOf<String, String>(
        )


        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value.isNotEmpty() }

        val headerParams: Map<String, String> = mapOf(
        )

        return suspendCoroutine { continuation ->
            val responseListener = Response.Listener<Unit> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<Unit>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<Unit> = requestFactory.build(
                    Request.Method.DELETE,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    formParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            postProcessors.forEach{ it.invoke(request)}

            requestQueue.value.add(request)
        }
    }
    /**
    * Returns pet inventories by status
    * Returns a map of status codes to quantities
       * @return kotlin.collections.Map<kotlin.String, kotlin.Int>
    */
    suspend fun getInventory(): kotlin.collections.Map<kotlin.String, kotlin.Int>? {
        val body: Any? = null

        val contentTypes : Array<String> = arrayOf()
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/store/inventory";

        // form params
        val formParams = mapOf<String, String>(
        )


        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value.isNotEmpty() }

        val headerParams: Map<String, String> = mapOf(
        )

        return suspendCoroutine { continuation ->
            val responseListener = Response.Listener<kotlin.collections.Map<kotlin.String, kotlin.Int>> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<kotlin.collections.Map<kotlin.String, kotlin.Int>>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<kotlin.collections.Map<kotlin.String, kotlin.Int>> = requestFactory.build(
                    Request.Method.GET,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    formParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            postProcessors.forEach{ it.invoke(request)}

            requestQueue.value.add(request)
        }
    }
    /**
    * Find purchase order by ID
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
       * @param orderId ID of pet that needs to be fetched
       * @return Order
    */
    suspend fun getOrderById(orderId: kotlin.Long): Order? {
        val body: Any? = null
        // verify the required parameter 'orderId' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(orderId)

        val contentTypes : Array<String> = arrayOf()
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/store/order/{orderId}".replace("{" + "orderId" + "}", IRequestFactory.escapeString(orderId.toString()));

        // form params
        val formParams = mapOf<String, String>(
        )


        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value.isNotEmpty() }

        val headerParams: Map<String, String> = mapOf(
        )

        return suspendCoroutine { continuation ->
            val responseListener = Response.Listener<Order> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<Order>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<Order> = requestFactory.build(
                    Request.Method.GET,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    formParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            postProcessors.forEach{ it.invoke(request)}

            requestQueue.value.add(request)
        }
    }
    /**
    * Place an order for a pet
    * 
       * @param body order placed for purchasing the pet
       * @return Order
    */
    suspend fun placeOrder(body: Order): Order? {
        val body: Any? = body
        // verify the required parameter 'body' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(body)

        val contentTypes : Array<String> = arrayOf()
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/store/order";

        // form params
        val formParams = mapOf<String, String>(
        )


        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value.isNotEmpty() }

        val headerParams: Map<String, String> = mapOf(
        )

        return suspendCoroutine { continuation ->
            val responseListener = Response.Listener<Order> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<Order>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<Order> = requestFactory.build(
                    Request.Method.POST,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    formParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            postProcessors.forEach{ it.invoke(request)}

            requestQueue.value.add(request)
        }
    }
}
