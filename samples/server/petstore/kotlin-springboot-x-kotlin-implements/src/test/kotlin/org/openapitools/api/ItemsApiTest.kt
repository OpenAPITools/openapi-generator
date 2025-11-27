package org.openapitools.api

import org.openapitools.model.ItemWithDollarAttributesAndExamples
import org.openapitools.model.ItemsItemIdGet200Response
import org.junit.jupiter.api.Test
import org.springframework.http.ResponseEntity

class ItemsApiTest {

    private val api: ItemsApiController = ItemsApiController()

    /**
     * To test ItemsApiController.itemsItemIdGet
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun itemsItemIdGetTest() {
        val itemDollarId: kotlin.String = TODO()
        val filterDollarType: kotlin.String = TODO()
        val xCustomDollarHeader: kotlin.String? = TODO()
        val sessionDollarToken: kotlin.String? = TODO()
        val request: javax.servlet.http.HttpServletRequest = TODO()
        val response: ResponseEntity<ItemsItemIdGet200Response> = api.itemsItemIdGet(itemDollarId, filterDollarType, xCustomDollarHeader, sessionDollarToken, request)

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
        val xPostDollarHeader: kotlin.String? = TODO()
        val formDollarName: kotlin.String? = TODO()
        val formDollarValue: kotlin.String? = TODO()
        val request: javax.servlet.http.HttpServletRequest = TODO()
        val response: ResponseEntity<ItemWithDollarAttributesAndExamples> = api.itemsPost(xPostDollarHeader, formDollarName, formDollarValue, request)

        // TODO: test validations
    }
}
