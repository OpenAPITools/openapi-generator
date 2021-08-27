package org.openapitools.client.apis

import android.content.Context
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

import org.openapitools.client.models.ApiResponse
import org.openapitools.client.models.Pet

/*
* If you wish to use a custom http stack with your client you
* can pass that to the request queue like:
* Volley.newRequestQueue(context.applicationContext, myCustomHttpStack)
*/
class PetApi (
    val context: Context,
    val requestQueue: Lazy<RequestQueue> = lazy(initializer = {
        Volley.newRequestQueue(context.applicationContext)
    }),
    val requestFactory: IRequestFactory = RequestFactory(),
    val basePath: String = "http://petstore.swagger.io/v2") {

    /**
    * Add a new pet to the store
    * 
       * @param body Pet object that needs to be added to the store
       * @return void
    */
    suspend fun addPet(body: Pet): Unit {
        var body: Any? = body
        // verify the required parameter 'body' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(body)

        var contentTypes : Array<String> = arrayOf("application/json","application/xml")
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/pet";

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

            requestQueue.value.add(request)
        }
    }
    /**
    * Deletes a pet
    * 
       * @param petId Pet id to delete
       * @param apiKey 
       * @return void
    */
    suspend fun deletePet(petId: kotlin.Long, apiKey: kotlin.String? = null): Unit {
        var body: Any? = null
        // verify the required parameter 'petId' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(petId)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/pet/{petId}".replace("{" + "petId" + "}", IRequestFactory.escapeString(petId.toString()));

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
            "api_key" to IRequestFactory.parameterToString(apiKey),
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

            requestQueue.value.add(request)
        }
    }
    /**
    * Finds Pets by status
    * Multiple status values can be provided with comma separated strings
       * @param status Status values that need to be considered for filter
       * @return kotlin.collections.List<Pet>
    */
    suspend fun findPetsByStatus(status: CSVParams): kotlin.collections.List<Pet>? {
        var body: Any? = null
        // verify the required parameter 'status' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(status)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/pet/findByStatus";

        // form params
        // TODO: Comment this back in and use them when using form parameters
        // TODO: Investigate how form parameters are used in volley
        // val formParams: Map<String, String> = HashMap()

        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
            "status" to IRequestFactory.parameterToString(status),
        ).filter { it.value != ""}

        val headerParams: Map<String, String> = mapOf(
        )

        return suspendCoroutine { continuation ->
            val responseListener = Response.Listener<kotlin.collections.List<Pet>> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<kotlin.collections.List<Pet>>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<kotlin.collections.List<Pet>> = requestFactory.build(
                    Request.Method.GET,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            requestQueue.value.add(request)
        }
    }
    /**
    * Finds Pets by tags
    * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
       * @param tags Tags to filter by
       * @return kotlin.collections.List<Pet>
    */
    @Deprecated("This api was deprecated")
    suspend fun findPetsByTags(tags: CSVParams): kotlin.collections.List<Pet>? {
        var body: Any? = null
        // verify the required parameter 'tags' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(tags)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/pet/findByTags";

        // form params
        // TODO: Comment this back in and use them when using form parameters
        // TODO: Investigate how form parameters are used in volley
        // val formParams: Map<String, String> = HashMap()

        // TODO: Cater for allowing empty values
        // TODO, if its apikey auth, then add the header names here and the hardcoded auth key
        // Only support hard coded apikey in query param auth for when we do this first path
        val queryParams = mapOf<String, String>(
            "tags" to IRequestFactory.parameterToString(tags),
        ).filter { it.value != ""}

        val headerParams: Map<String, String> = mapOf(
        )

        return suspendCoroutine { continuation ->
            val responseListener = Response.Listener<kotlin.collections.List<Pet>> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<kotlin.collections.List<Pet>>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<kotlin.collections.List<Pet>> = requestFactory.build(
                    Request.Method.GET,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            requestQueue.value.add(request)
        }
    }
    /**
    * Find pet by ID
    * Returns a single pet
       * @param petId ID of pet to return
       * @return Pet
    */
    suspend fun getPetById(petId: kotlin.Long): Pet? {
        var body: Any? = null
        // verify the required parameter 'petId' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(petId)

        var contentTypes : Array<String> = arrayOf()
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/pet/{petId}".replace("{" + "petId" + "}", IRequestFactory.escapeString(petId.toString()));

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
            val responseListener = Response.Listener<Pet> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<Pet>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<Pet> = requestFactory.build(
                    Request.Method.GET,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            requestQueue.value.add(request)
        }
    }
    /**
    * Update an existing pet
    * 
       * @param body Pet object that needs to be added to the store
       * @return void
    */
    suspend fun updatePet(body: Pet): Unit {
        var body: Any? = body
        // verify the required parameter 'body' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(body)

        var contentTypes : Array<String> = arrayOf("application/json","application/xml")
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/pet";

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

            requestQueue.value.add(request)
        }
    }
    /**
    * Updates a pet in the store with form data
    * 
       * @param petId ID of pet that needs to be updated
       * @param name Updated name of the pet
       * @param status Updated status of the pet
       * @return void
    */
    suspend fun updatePetWithForm(petId: kotlin.Long, @Field("name") name: kotlin.String? = null, @Field("status") status: kotlin.String? = null): Unit {
        var body: Any? = null
        // verify the required parameter 'petId' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(petId)

        var contentTypes : Array<String> = arrayOf("application/x-www-form-urlencoded")
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/pet/{petId}".replace("{" + "petId" + "}", IRequestFactory.escapeString(petId.toString()));

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

            requestQueue.value.add(request)
        }
    }
    /**
    * uploads an image
    * 
       * @param petId ID of pet to update
       * @param additionalMetadata Additional data to pass to server
       * @param file file to upload
       * @return ApiResponse
    */
    suspend fun uploadFile(petId: kotlin.Long, @Part("additionalMetadata") additionalMetadata: kotlin.String? = null, @Part file: MultipartBody.Part? = null): ApiResponse? {
        var body: Any? = null
        // verify the required parameter 'petId' is set
        // This is probably taken care of by non-null types anyway
        requireNotNull(petId)

        var contentTypes : Array<String> = arrayOf("multipart/form-data")
        var contentType: String = if (contentTypes.size > 0) { contentTypes[0] } else { "application/json" }

        // Do some work or avoid some work based on what we know about the model, 
        // before we delegate to a pluggable request factory template
        // The request factory template contains only pure code and no templates
        // to make it easy to override with your own.

        // create path and map variables
        val path = "/pet/{petId}/uploadImage".replace("{" + "petId" + "}", IRequestFactory.escapeString(petId.toString()));

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
            val responseListener = Response.Listener<ApiResponse> { response ->
                continuation.resume(response)
            }

            val errorListener = Response.ErrorListener { error ->
                continuation.resumeWithException(error)
            }

            val responseType = object : TypeToken<ApiResponse>() {}.type

            // Call the correct request builder based on whether we have a return type or a body.
            // All other switching on types must be done in code inside the builder
            val request: Request<ApiResponse> = requestFactory.build(
                    Request.Method.POST,
                    "$basePath$path",
                    body,
                    headerParams,
                    queryParams,
                    contentType,
                    responseType,
                    responseListener,
                    errorListener)

            requestQueue.value.add(request)
        }
    }
}
