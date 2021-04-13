package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody

import org.openapitools.client.models.Client

interface FakeClassnameTags123Api {
    /**
     * To test class name in snake case
     * To test class name in snake case
     * Responses:
     *  - 200: successful operation
     * 
     * @param client client model 
     * @return [Client]
     */
    @PATCH("fake_classname_test")
    suspend fun testClassname(@Body client: Client): Response<Client>

}
