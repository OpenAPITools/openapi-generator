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
    private val basePath: String = "http://localhost",
    private val postProcessors :List <(Request<*>) -> Unit> = listOf()) {

    /**
     * Tests default values
     * Tests default values of different parameters
     * @param pi0  (default to 10)
     * @param pi1 
     * @param pn0  (default to 10.0)
     * @param pn1 
     * @param qi0  (optional, default to 10)
     * @param qi1  (default to 71)
     * @param qi2  (optional)
     * @param qi3 
     * @param qn0  (optional, default to 10.0)
     * @param qn1  (default to 71.0)
     * @param qn2  (optional)
     * @param qn3 
     * @param hi0  (optional, default to 10)
     * @param hi1  (default to 71)
     * @param hi2  (optional)
     * @param hi3 
     * @param hn0  (optional, default to 10.0)
     * @param hn1  (default to 71.0)
     * @param hn2  (optional)
     * @param hn3 
     * @param fi0  (optional, default to 10)
     * @param fi1  (default to 71)
     * @param fi2  (optional)
     * @param fi3 
     * @param fn0  (optional, default to 10.0)
     * @param fn1  (default to 71.0)
     * @param fn2  (optional)
     * @param fn3 
     * @param fn4 
     * @return void
     */
    suspend fun test(pi0: kotlin.Int = 10, pi1: kotlin.Int, pn0: java.math.BigDecimal = java.math.BigDecimal("10.0"), pn1: java.math.BigDecimal, qi0: kotlin.Int? = 10, qi1: kotlin.Int = 71, qi2: kotlin.Int? = null, qi3: kotlin.Int, qn0: java.math.BigDecimal? = java.math.BigDecimal("10.0"), qn1: java.math.BigDecimal = java.math.BigDecimal("71.0"), qn2: java.math.BigDecimal? = null, qn3: java.math.BigDecimal, hi0: kotlin.Int? = 10, hi1: kotlin.Int = 71, hi2: kotlin.Int? = null, hi3: kotlin.Int, hn0: java.math.BigDecimal? = java.math.BigDecimal("10.0"), hn1: java.math.BigDecimal = java.math.BigDecimal("71.0"), hn2: java.math.BigDecimal? = null, hn3: java.math.BigDecimal, fi0: kotlin.Int? = 10, fi1: kotlin.Int = 71, fi2: kotlin.Int? = null, fi3: kotlin.Int, fn0: java.math.BigDecimal? = java.math.BigDecimal("10.0"), fn1: java.math.BigDecimal = java.math.BigDecimal("71.0"), fn2: java.math.BigDecimal? = null, fn3: java.math.BigDecimal, fn4: kotlin.collections.List<kotlin.String>): Unit {
        val body: Any? = null

        val contentTypes : Array<String> = arrayOf("multipart/form-data")
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model,
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/test".replace("{" + "pi0" + "}", IRequestFactory.escapeString(pi0.toString())).replace("{" + "pi1" + "}", IRequestFactory.escapeString(pi1.toString())).replace("{" + "pn0" + "}", IRequestFactory.escapeString(pn0.toString())).replace("{" + "pn1" + "}", IRequestFactory.escapeString(pn1.toString()))

        val formParams = mapOf<String, String>(
            "fi0" to IRequestFactory.parameterToString(fi0),
            "fi1" to IRequestFactory.parameterToString(fi1),
            "fi2" to IRequestFactory.parameterToString(fi2),
            "fi3" to IRequestFactory.parameterToString(fi3),
            "fn0" to IRequestFactory.parameterToString(fn0),
            "fn1" to IRequestFactory.parameterToString(fn1),
            "fn2" to IRequestFactory.parameterToString(fn2),
            "fn3" to IRequestFactory.parameterToString(fn3),
            "fn4" to IRequestFactory.parameterToString(fn4)
        )


        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
                "qi0" to IRequestFactory.parameterToString(qi0),
                "qi1" to IRequestFactory.parameterToString(qi1),
                "qi2" to IRequestFactory.parameterToString(qi2),
                "qi3" to IRequestFactory.parameterToString(qi3),
                "qn0" to IRequestFactory.parameterToString(qn0),
                "qn1" to IRequestFactory.parameterToString(qn1),
                "qn2" to IRequestFactory.parameterToString(qn2),
                "qn3" to IRequestFactory.parameterToString(qn3)
            )
            .filter { it.value.isNotEmpty() }

        val headerParams: Map<String, String> = mapOf(
            "hi0" to IRequestFactory.parameterToString(hi0),
            "hi1" to IRequestFactory.parameterToString(hi1),
            "hi2" to IRequestFactory.parameterToString(hi2),
            "hi3" to IRequestFactory.parameterToString(hi3),
            "hn0" to IRequestFactory.parameterToString(hn0),
            "hn1" to IRequestFactory.parameterToString(hn1),
            "hn2" to IRequestFactory.parameterToString(hn2),
            "hn3" to IRequestFactory.parameterToString(hn3)
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
