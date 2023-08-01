package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call
import okhttp3.RequestBody


interface DefaultApi {
    /**
     * 
     * 
     * Responses:
     *  - 200: Successful operation
     *
     * @param ids  
     * @return [Call]<[Unit]>
     */
    @GET("{ids}")
    fun idsGet(@Path("ids") ids: kotlin.collections.List<kotlin.String>): Call<Unit>

}
