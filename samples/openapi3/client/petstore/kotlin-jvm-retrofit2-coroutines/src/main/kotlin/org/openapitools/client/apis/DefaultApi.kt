package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody

import org.openapitools.client.models.InlineResponseDefault

interface DefaultApi {
    /**
     * 
     * 
     * Responses:
     *  - 0: response
     * 
     * @return [InlineResponseDefault]
     */
    @GET("foo")
    suspend fun fooGet(): Response<InlineResponseDefault>

}
