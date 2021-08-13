package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import rx.Observable

import org.openapitools.client.models.Client

interface AnotherFakeApi {
    /**
     * To test special tags
     * To test special tags and operation ID starting with number
     * Responses:
     *  - 200: successful operation
     * 
     * @param client client model 
     * @return [Call]<[Client]>
     */
    @PATCH("another-fake/dummy")
    fun call123testSpecialTags(@Body client: Client): Observable<Client>

}
