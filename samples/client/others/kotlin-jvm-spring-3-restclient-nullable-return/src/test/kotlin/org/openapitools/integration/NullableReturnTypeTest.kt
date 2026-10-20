package org.openapitools.integration

import io.kotlintest.shouldBe
import io.kotlintest.specs.ShouldSpec
import org.openapitools.client.apis.DefaultApi
import org.openapitools.client.models.PingRequest
import org.springframework.boot.test.web.client.MockServerRestClientCustomizer
import org.springframework.http.HttpMethod
import org.springframework.http.MediaType.TEXT_HTML
import org.springframework.test.web.client.match.MockRestRequestMatchers.method
import org.springframework.test.web.client.match.MockRestRequestMatchers.requestTo
import org.springframework.test.web.client.response.MockRestResponseCreators.withSuccess
import org.springframework.web.client.RestClient

class NullableReturnTypeTest : ShouldSpec() {
    init {
        should("return null when the response body is empty and the return type is nullable") {
            val restClientBuilder: RestClient.Builder = RestClient.builder()
            val mockServer = MockServerRestClientCustomizer().let {
                it.customize(restClientBuilder)
                it.getServer(restClientBuilder)
            }
            val api = DefaultApi(restClientBuilder.build())

            mockServer.expect(requestTo("/nullable-string"))
                .andExpect(method(HttpMethod.POST))
                .andRespond(withSuccess("", TEXT_HTML))

            val response = api.returnNullableString(PingRequest(msg = "hello"))

            mockServer.verify()

            response shouldBe null
        }

        should("return the body when the response has one and the return type is nullable") {
            val restClientBuilder: RestClient.Builder = RestClient.builder()
            val mockServer = MockServerRestClientCustomizer().let {
                it.customize(restClientBuilder)
                it.getServer(restClientBuilder)
            }
            val api = DefaultApi(restClientBuilder.build())

            mockServer.expect(requestTo("/nullable-string"))
                .andExpect(method(HttpMethod.POST))
                .andRespond(withSuccess("<html>ok</html>", TEXT_HTML))

            val response = api.returnNullableString(PingRequest(msg = "hello"))

            mockServer.verify()

            response shouldBe "<html>ok</html>"
        }
    }
}
