package org.openapitools.api

import org.openapitools.model.ParentSchema
import org.junit.jupiter.api.Test
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.test.runBlockingTest
import org.springframework.http.ResponseEntity

class DocumentsApiTest {

    private val service: DocumentsApiService = DocumentsApiServiceImpl()
    private val api: DocumentsApiController = DocumentsApiController(service)

    /**
     * To test DocumentsApiController.documentsV1Get
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun documentsV1GetTest() = runBlockingTest {
        val response: ResponseEntity<Flow<ParentSchema>> = api.documentsV1Get()

        // TODO: test validations
    }
}
