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

import org.openapitools.client.models.User

/*
* If you wish to use a custom http stack with your client you
* can pass that to the request queue like:
* Volley.newRequestQueue(context.applicationContext, myCustomHttpStack)
*/
class UserApi (
    val context: Context,
    val requestQueue: Lazy<RequestQueue> = lazy(initializer = {
        Volley.newRequestQueue(context.applicationContext)
    }),
    val requestFactory: IRequestFactory = RequestFactory(),
    val basePath: String = "http://petstore.swagger.io/v2",
    val retryPolicy: DefaultRetryPolicy? = null) {

    /**
    * Create user
    * This can only be done by the logged in user.
       * @param body Created user object
       * @return void
    */
    suspend fun createUser(body: User): Unit {
        var body: Any? = body
        // verify the required parameter 'body' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(body)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user";

        // form params
        // TODO: Comment this back in and use them when using form parameters
        // TODO: Investigate how form parameters are used in volley
        // val formParams: Map<String, String> = HashMap()

        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value != ""}

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
                    Request.Method.POST,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            if (retryPolicy != null) {
                request.retryPolicy = retryPolicy
            }

            requestQueue.value.add(request)
        }
    }
    /**
    * Creates list of users with given input array
    * 
       * @param body List of user object
       * @return void
    */
    suspend fun createUsersWithArrayInput(body: kotlin.collections.List<User>): Unit {
        var body: Any? = body
        // verify the required parameter 'body' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(body)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/createWithArray";

        // form params
        // TODO: Comment this back in and use them when using form parameters
        // TODO: Investigate how form parameters are used in volley
        // val formParams: Map<String, String> = HashMap()

        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value != ""}

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
                    Request.Method.POST,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            if (retryPolicy != null) {
                request.retryPolicy = retryPolicy
            }

            requestQueue.value.add(request)
        }
    }
    /**
    * Creates list of users with given input array
    * 
       * @param body List of user object
       * @return void
    */
    suspend fun createUsersWithListInput(body: kotlin.collections.List<User>): Unit {
        var body: Any? = body
        // verify the required parameter 'body' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(body)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/createWithList";

        // form params
        // TODO: Comment this back in and use them when using form parameters
        // TODO: Investigate how form parameters are used in volley
        // val formParams: Map<String, String> = HashMap()

        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value != ""}

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
                    Request.Method.POST,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            if (retryPolicy != null) {
                request.retryPolicy = retryPolicy
            }

            requestQueue.value.add(request)
        }
    }
    /**
    * Delete user
    * This can only be done by the logged in user.
       * @param username The name that needs to be deleted
       * @return void
    */
    suspend fun deleteUser(username: kotlin.String): Unit {
        var body: Any? = null
        // verify the required parameter 'username' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(username)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/{username}".replace("{" + "username" + "}", IRequestFactory.escapeString(username.toString()));

        // form params
        // TODO: Comment this back in and use them when using form parameters
        // TODO: Investigate how form parameters are used in volley
        // val formParams: Map<String, String> = HashMap()

        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value != ""}

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
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            if (retryPolicy != null) {
                request.retryPolicy = retryPolicy
            }

            requestQueue.value.add(request)
        }
    }
    /**
    * Get user by user name
    * 
       * @param username The name that needs to be fetched. Use user1 for testing.
       * @return User
    */
    suspend fun getUserByName(username: kotlin.String): User? {
        var body: Any? = null
        // verify the required parameter 'username' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(username)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/{username}".replace("{" + "username" + "}", IRequestFactory.escapeString(username.toString()));

        // form params
        // TODO: Comment this back in and use them when using form parameters
        // TODO: Investigate how form parameters are used in volley
        // val formParams: Map<String, String> = HashMap()

        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value != ""}

        val headerParams: Map<String, String> = mapOf(
        )

        return suspendCoroutine { continuation ->
            val responseListener = Response.Listener<User> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<User>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<User> = requestFactory.build(
                    Request.Method.GET,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            if (retryPolicy != null) {
                request.retryPolicy = retryPolicy
            }

            requestQueue.value.add(request)
        }
    }
    /**
    * Logs user into the system
    * 
       * @param username The user name for login
       * @param password The password for login in clear text
       * @return kotlin.String
    */
    suspend fun loginUser(username: kotlin.String, password: kotlin.String): kotlin.String? {
        var body: Any? = null
        // verify the required parameter 'username' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(username)
        // verify the required parameter 'password' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(password)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/login";

        // form params
        // TODO: Comment this back in and use them when using form parameters
        // TODO: Investigate how form parameters are used in volley
        // val formParams: Map<String, String> = HashMap()

        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
            "username" to IRequestFactory.parameterToString(username),
            "password" to IRequestFactory.parameterToString(password),
        ).filter { it.value != ""}

        val headerParams: Map<String, String> = mapOf(
        )

        return suspendCoroutine { continuation ->
            val responseListener = Response.Listener<kotlin.String> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<kotlin.String>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<kotlin.String> = requestFactory.build(
                    Request.Method.GET,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            if (retryPolicy != null) {
                request.retryPolicy = retryPolicy
            }

            requestQueue.value.add(request)
        }
    }
    /**
    * Logs out current logged in user session
    * 
       * @return void
    */
    suspend fun logoutUser(): Unit {
        var body: Any? = null

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/logout";

        // form params
        // TODO: Comment this back in and use them when using form parameters
        // TODO: Investigate how form parameters are used in volley
        // val formParams: Map<String, String> = HashMap()

        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value != ""}

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
                    Request.Method.GET,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            if (retryPolicy != null) {
                request.retryPolicy = retryPolicy
            }

            requestQueue.value.add(request)
        }
    }
    /**
    * Updated user
    * This can only be done by the logged in user.
       * @param username name that need to be deleted
       * @param body Updated user object
       * @return void
    */
    suspend fun updateUser(username: kotlin.String, body: User): Unit {
        var body: Any? = body
        // verify the required parameter 'username' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(username)
        // verify the required parameter 'body' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(body)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/{username}".replace("{" + "username" + "}", IRequestFactory.escapeString(username.toString()));

        // form params
        // TODO: Comment this back in and use them when using form parameters
        // TODO: Investigate how form parameters are used in volley
        // val formParams: Map<String, String> = HashMap()

        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
        ).filter { it.value != ""}

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
                    Request.Method.PUT,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            if (retryPolicy != null) {
                request.retryPolicy = retryPolicy
            }

            requestQueue.value.add(request)
        }
    }
}
