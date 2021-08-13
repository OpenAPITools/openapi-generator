package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody

import org.openapitools.client.models.Client

interface AnotherFakeApi {
    /**
     * To test special tags
     * To test special tags and operation ID starting with number
     * Responses:
     *  - 200: successful operation
     * 
     * @param client client model 
     * @return [Client]
     */
    @PATCH("another-fake/dummy")
    suspend fun call123testSpecialTags(@Body client: Client): Response<Client>

}
