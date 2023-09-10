package org.openapitools.api

import org.openapitools.model.Annotation
import org.junit.jupiter.api.Test
import org.springframework.http.ResponseEntity

class FakeApiTest {

    private val api: FakeApiController = FakeApiController()

    /**
     * To test FakeApiController.annotations
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun annotationsTest() {
        val `annotation`: Annotation = TODO()
        val response: ResponseEntity<Unit> = api.annotations(`annotation`)

        // TODO: test validations
    }
}
