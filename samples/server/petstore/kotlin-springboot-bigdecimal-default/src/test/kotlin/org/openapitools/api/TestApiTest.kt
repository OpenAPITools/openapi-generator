package org.openapitools.api

import org.openapitools.model.Apa
import org.junit.jupiter.api.Test
import org.springframework.http.ResponseEntity

class TestApiTest {

    private val api: TestApiController = TestApiController()

    /**
     * To test TestApiController.testPost
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun testPostTest() {
        val apa: Apa = TODO()
        val response: ResponseEntity<Unit> = api.testPost(apa)

        // TODO: test validations
    }
}
