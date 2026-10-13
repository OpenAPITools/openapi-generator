package org.openapitools.api

import org.openapitools.model.Annotation
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Disabled
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
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
    fun annotationsTest() {
        val `annotation`: Annotation = TODO()
        val response: ResponseEntity<Unit> = api.annotations(`annotation`)

        // TODO: test validations
    }
}
