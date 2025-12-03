package org.openapitools.api

import org.openapitools.model.ItemWithDollarAttributesAndExamples
import org.openapitools.model.ItemsItemIdSomethingItemSubIdGet200Response
import org.junit.jupiter.api.Test
import org.springframework.http.ResponseEntity

class ItemsApiTest {

    private val service: ItemsApiService = ItemsApiServiceImpl()
    private val api: ItemsApiController = ItemsApiController(service)

    /**
     * To test ItemsApiController.itemsItemIdSomethingItemSubIdGet
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun itemsItemIdSomethingItemSubIdGetTest() {
        val itemDollarId: kotlin.String = TODO()
        val itemDollarSubId: kotlin.String = TODO()
        val filterDollarType: kotlin.String = TODO()
        val filterDollarSubType: kotlin.String = TODO()
        val xCustomHeader: kotlin.String? = TODO()
        val xCustomHeaderTwo: kotlin.String? = TODO()
        val sessionDollarToken: kotlin.String? = TODO()
        val sessionDollarTokenTwo: kotlin.String? = TODO()
        
        val response: ResponseEntity<ItemsItemIdSomethingItemSubIdGet200Response> = api.itemsItemIdSomethingItemSubIdGet(itemDollarId, itemDollarSubId, filterDollarType, filterDollarSubType, xCustomHeader, xCustomHeaderTwo, sessionDollarToken, sessionDollarTokenTwo)

        // TODO: test validations
    }

    /**
     * To test ItemsApiController.itemsPost
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun itemsPostTest() {
        val xPostHeader: kotlin.String? = TODO()
        val formDollarName: kotlin.String? = TODO()
        val formDollarValue: kotlin.String = TODO()
        
        val response: ResponseEntity<ItemWithDollarAttributesAndExamples> = api.itemsPost(xPostHeader, formDollarName, formDollarValue)

        // TODO: test validations
    }
}
