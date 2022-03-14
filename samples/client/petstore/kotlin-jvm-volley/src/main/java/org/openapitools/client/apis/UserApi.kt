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

import org.openapitools.client.models.User

/*
* If you wish to use a custom http stack with your client you
* can pass that to the request queue like:
* Volley.newRequestQueue(context.applicationContext, myCustomHttpStack)
*/
class UserApi (
    private val context: Context,
    private val requestQueue: Lazy<RequestQueue> = lazy(initializer = {
        Volley.newRequestQueue(context.applicationContext)
    }),
    private val requestFactory: IRequestFactory = RequestFactory(),
    private val basePath: String = "http://petstore.swagger.io/v2",
    private val postProcessors :List <(Request<*>) -> Unit> = listOf()) {

    /**
    * Create user
    * This can only be done by the logged in user.
       * @param body Created user object
       * @return void
    */
    suspend fun createUser(body: User): Unit {
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
        val path = "/user";

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
    /**
    * Creates list of users with given input array
    * 
       * @param body List of user object
       * @return void
    */
    suspend fun createUsersWithArrayInput(body: kotlin.collections.List<User>): Unit {
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
        val path = "/user/createWithArray";

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
    /**
    * Creates list of users with given input array
    * 
       * @param body List of user object
       * @return void
    */
    suspend fun createUsersWithListInput(body: kotlin.collections.List<User>): Unit {
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
        val path = "/user/createWithList";

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
    /**
    * Delete user
    * This can only be done by the logged in user.
       * @param username The name that needs to be deleted
       * @return void
    */
    suspend fun deleteUser(username: kotlin.String): Unit {
        val body: Any? = null
        // verify the required parameter 'username' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(username)

        val contentTypes : Array<String> = arrayOf()
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/{username}".replace("{" + "username" + "}", IRequestFactory.escapeString(username.toString()));

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
    * Get user by user name
    * 
       * @param username The name that needs to be fetched. Use user1 for testing.
       * @return User
    */
    suspend fun getUserByName(username: kotlin.String): User? {
        val body: Any? = null
        // verify the required parameter 'username' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(username)

        val contentTypes : Array<String> = arrayOf()
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/{username}".replace("{" + "username" + "}", IRequestFactory.escapeString(username.toString()));

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
    * Logs user into the system
    * 
       * @param username The user name for login
       * @param password The password for login in clear text
       * @return kotlin.String
    */
    suspend fun loginUser(username: kotlin.String, password: kotlin.String): kotlin.String? {
        val body: Any? = null
        // verify the required parameter 'username' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(username)
        // verify the required parameter 'password' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(password)

        val contentTypes : Array<String> = arrayOf()
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/login";

        // form params
        val formParams = mapOf<String, String>(
        )


        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
            "username" to IRequestFactory.parameterToString(username),
            "password" to IRequestFactory.parameterToString(password),
        ).filter { it.value.isNotEmpty() }

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
    * Logs out current logged in user session
    * 
       * @return void
    */
    suspend fun logoutUser(): Unit {
        val body: Any? = null

        val contentTypes : Array<String> = arrayOf()
        val contentType: String = if (contentTypes.isNotEmpty()) { contentTypes.first() } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/user/logout";

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
    * Updated user
    * This can only be done by the logged in user.
       * @param username name that need to be deleted
       * @param body Updated user object
       * @return void
    */
    suspend fun updateUser(username: kotlin.String, body: User): Unit {
        val body: Any? = body
        // verify the required parameter 'username' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(username)
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
        val path = "/user/{username}".replace("{" + "username" + "}", IRequestFactory.escapeString(username.toString()));

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
                    Request.Method.PUT,
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
