package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import rx.Observable

import org.openapitools.client.models.InlineResponseDefault

interface DefaultApi {
    /**
     * 
     * 
     * Responses:
     *  - 0: response
     * 
     * @return [Call]<[InlineResponseDefault]>
     */
    @GET("foo")
    fun fooGet(): Observable<InlineResponseDefault>

}
