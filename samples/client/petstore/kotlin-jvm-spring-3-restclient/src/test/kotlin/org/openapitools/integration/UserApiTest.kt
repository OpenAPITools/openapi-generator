package org.openapitools.integration

import io.kotlintest.shouldBe
import io.kotlintest.specs.ShouldSpec
import org.openapitools.client.apis.UserApi
import org.springframework.boot.test.web.client.MockServerRestClientCustomizer
import org.springframework.http.HttpMethod
import org.springframework.http.MediaType.TEXT_PLAIN
import org.springframework.test.web.client.match.MockRestRequestMatchers.method
import org.springframework.test.web.client.match.MockRestRequestMatchers.requestTo
import org.springframework.test.web.client.response.MockRestResponseCreators.withSuccess
import org.springframework.web.client.RestClient

class UserApiTest : ShouldSpec() {
    init {
        should("call login user passing a query parameter with a single value") {
            val restClientBuilder: RestClient.Builder = RestClient.builder()
            val mockServer = MockServerRestClientCustomizer().let {
                it.customize(restClientBuilder)
                it.getServer(restClientBuilder)
            }
            val userApi = UserApi(restClientBuilder.build())

            mockServer.expect(requestTo("/user/login?username=myUsername&password=myPassword"))
                .andExpect(method(HttpMethod.GET))
                .andRespond(withSuccess("login response", TEXT_PLAIN))

            val response = userApi.loginUser("myUsername", "myPassword")

            mockServer.verify()

            response shouldBe "login response"
        }
    }
}
