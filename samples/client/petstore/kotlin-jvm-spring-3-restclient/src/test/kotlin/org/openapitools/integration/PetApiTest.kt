package org.openapitools.integration

import io.kotlintest.matchers.collections.shouldHaveSize
import io.kotlintest.specs.ShouldSpec
import org.openapitools.client.apis.PetApi
import org.springframework.boot.test.web.client.MockServerRestClientCustomizer
import org.springframework.http.HttpMethod
import org.springframework.http.MediaType.APPLICATION_JSON
import org.springframework.test.web.client.match.MockRestRequestMatchers.method
import org.springframework.test.web.client.match.MockRestRequestMatchers.requestTo
import org.springframework.test.web.client.response.MockRestResponseCreators.withSuccess
import org.springframework.web.client.RestClient

class PetApiTest : ShouldSpec() {
    init {
        should("find by status passing a query parameter with a list of values") {
            val restClientBuilder: RestClient.Builder = RestClient.builder()
            val mockServer = MockServerRestClientCustomizer().let {
                it.customize(restClientBuilder)
                it.getServer(restClientBuilder)
            }
            val petApi = PetApi(restClientBuilder.build())

            mockServer.expect(requestTo("/pet/findByStatus?status=pending,available"))
                .andExpect(method(HttpMethod.GET))
                .andRespond(withSuccess("[]", APPLICATION_JSON))

            val response = petApi.findPetsByStatus(listOf(PetApi.StatusFindPetsByStatus.pending, PetApi.StatusFindPetsByStatus.available))

            mockServer.verify()

            response shouldHaveSize 0
        }
    }
}
