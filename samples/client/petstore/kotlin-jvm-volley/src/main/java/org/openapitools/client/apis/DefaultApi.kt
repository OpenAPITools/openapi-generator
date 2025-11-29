package org.openapitools.client.apis

import android.content.Context
import com.android.volley.DefaultRetryPolicy
import com.android.volley.Request
import com.android.volley.RequestQueue
import com.android.volley.Response
import com.android.volley.toolbox.BaseHttpStack
import com.android.volley.toolbox.Volley
import java.util.*
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException
import kotlin.coroutines.suspendCoroutine
import com.google.gson.reflect.TypeToken

import org.openapitools.client.request.IRequestFactory
import org.openapitools.client.request.RequestFactory
import org.openapitools.client.infrastructure.CollectionFormats.*

import org.openapitools.client.models.ItemWithDollarAttributesAndExamples
import org.openapitools.client.models.ItemsItemIdSomethingItemSubIdGet200Response

/*
* If you wish to use a custom http stack with your client you
* can pass that to the request queue like:
* Volley.newRequestQueue(context.applicationContext, myCustomHttpStack)
*/
class DefaultApi (
    private val context: Context,
    private val requestQueue: Lazy<RequestQueue> = lazy(initializer = {
        Volley.newRequestQueue(context.applicationContext)
    }),
    private val requestFactory: IRequestFactory = RequestFactory(),
    private val basePath: String = "http://petstore.swagger.io/v2",
    private val postProcessors :List <(Request<*>) -> Unit> = listOf()) {

    /**
     * GET /items/{item$Id}/something/{item$SubId}
     * SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * @param itemDollarId SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * @param itemDollarSubId SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * @param filterDollarType SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional, default to "SQ = \"; SBS = \\; DBS = \\\\; SD = $some")
     * @param filterDollarSubType SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional, default to "SQ = \"; SBS = \\; DBS = \\\\; SD = $some")
     * @param xCustomHeader SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @param xCustomHeaderTwo SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @return ItemsItemIdSomethingItemSubIdGet200Response
     */
    suspend fun itemsItemIdSomethingItemSubIdGet(itemDollarId: kotlin.String, itemDollarSubId: kotlin.String, filterDollarType: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", filterDollarSubType: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", xCustomHeader: kotlin.String? = null, xCustomHeaderTwo: kotlin.String? = null): ItemsItemIdSomethingItemSubIdGet200Response? {
        val body: Any? = null

        val contentTypes : Array<String> = arrayOf()
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model,
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/items/{item\$Id}/something/{item\$SubId}".replace("{" + "item\$Id" + "}", IRequestFactory.escapeString(itemDollarId.toString())).replace("{" + "item\$SubId" + "}", IRequestFactory.escapeString(itemDollarSubId.toString()))

        val formParams = mapOf<String, String>()


        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
                "filter\$Type" to IRequestFactory.parameterToString(filterDollarType),
                "filter\$SubType" to IRequestFactory.parameterToString(filterDollarSubType)
            )
            .filter { it.value.isNotEmpty() }

        val headerParams: Map<String, String> = mapOf(
            "X-Custom_Header" to IRequestFactory.parameterToString(xCustomHeader),
            "X-Custom_Header_two" to IRequestFactory.parameterToString(xCustomHeaderTwo)
        )

        return suspendCoroutine { continuation ->
            val responseListener = Response.Listener<ItemsItemIdSomethingItemSubIdGet200Response> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<ItemsItemIdSomethingItemSubIdGet200Response>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<ItemsItemIdSomethingItemSubIdGet200Response> = requestFactory.build(
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

            postProcessors.forEach { it.invoke(request) }

            requestQueue.value.add(request)
        }
    }
    /**
     * POST /items
     * SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * @param xPostHeader SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @param formDollarName SQ &#x3D; \\\&quot;; SBS &#x3D; \\\\; DBS &#x3D; \\\\\\\\; SD &#x3D; $some (optional)
     * @param formDollarValue SQ &#x3D; \\\&quot;; SBS &#x3D; \\\\; DBS &#x3D; \\\\\\\\; SD &#x3D; $some (optional, default to "SQ = \"; SBS = \\; DBS = \\\\; SD = $some")
     * @return ItemWithDollarAttributesAndExamples
     */
    suspend fun itemsPost(xPostHeader: kotlin.String? = null, formDollarName: kotlin.String? = null, formDollarValue: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = $some"): ItemWithDollarAttributesAndExamples? {
        val body: Any? = null

        val contentTypes : Array<String> = arrayOf("application/x-www-form-urlencoded")
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model,
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/items"

        val formParams = mapOf<String, String>(
            "form\$Name" to IRequestFactory.parameterToString(formDollarName),
            "form\$Value" to IRequestFactory.parameterToString(formDollarValue)
        )


        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>()
            .filter { it.value.isNotEmpty() }

        val headerParams: Map<String, String> = mapOf(
            "X-Post_Header" to IRequestFactory.parameterToString(xPostHeader)
        )

        return suspendCoroutine { continuation ->
            val responseListener = Response.Listener<ItemWithDollarAttributesAndExamples> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<ItemWithDollarAttributesAndExamples>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<ItemWithDollarAttributesAndExamples> = requestFactory.build(
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

            postProcessors.forEach { it.invoke(request) }

            requestQueue.value.add(request)
        }
    }
}
