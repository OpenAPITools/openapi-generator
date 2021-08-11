package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import rx.Observable

import org.openapitools.client.models.Client

interface FakeClassnameTags123Api {
    /**
     * To test class name in snake case
     * To test class name in snake case
     * Responses:
     *  - 200: successful operation
     * 
     * @param client client model 
     * @return [Call]<[Client]>
     */
    @PATCH("fake_classname_test")
    fun testClassname(@Body client: Client): Observable<Client>

}
