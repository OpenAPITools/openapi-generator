package org.openapitools.api

import org.openapitools.model.MultipartMixedRequestMarker
import org.openapitools.model.MultipartMixedStatus
import org.junit.jupiter.api.Test
import org.springframework.http.ResponseEntity

class MultipartMixedApiTest {

    private val api: MultipartMixedApiController = MultipartMixedApiController()

    /**
     * To test MultipartMixedApiController.multipartMixed
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun multipartMixedTest() {
        val status: MultipartMixedStatus = TODO()
        val file: org.springframework.web.multipart.MultipartFile = TODO()
        val marker: MultipartMixedRequestMarker? = TODO()
        val response: ResponseEntity<Unit> = api.multipartMixed(status, file, marker)

        // TODO: test validations
    }
}
