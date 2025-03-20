package org.openapitools.apis

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.kotlin.kotlinModule
import com.github.tomakehurst.wiremock.client.WireMock.equalTo
import com.github.tomakehurst.wiremock.junit5.WireMockRuntimeInfo
import com.github.tomakehurst.wiremock.junit5.WireMockTest
import org.openapitools.models.Pet
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import kotlin.test.Test
import kotlin.test.assertEquals

@WireMockTest
class PetApiStubsTest {

    private val objectMapper = ObjectMapper()
        .registerModules(JavaTimeModule())
        .registerModules(kotlinModule())

    private val stubs = PetApiStubs(objectMapper)

    private val client = HttpClient.newBuilder().build()

    @Test
    fun `stub and retrieve`(wiremock: WireMockRuntimeInfo) {
        val pet = Pet("Pet", emptyList(), null, null, null, null)

        wiremock.wireMock.register(
            stubs.getPetById(equalTo("2")).respondWith200(pet)
        )

        val request = HttpRequest.newBuilder(URI.create(wiremock.httpBaseUrl + "/pet/1"))
            .GET()
            .build()

        val response = client.send(request, HttpResponse.BodyHandlers.ofString())

        assertEquals(pet, objectMapper.readValue(response.body(), Pet::class.java))
    }
}