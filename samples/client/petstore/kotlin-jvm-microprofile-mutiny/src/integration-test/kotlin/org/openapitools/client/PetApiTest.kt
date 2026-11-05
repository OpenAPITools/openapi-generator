package org.openapitools.client

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.exc.InvalidTypeIdException
import com.fasterxml.jackson.databind.exc.MismatchedInputException
import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpServer
import io.quarkus.test.common.QuarkusTestResource
import io.quarkus.test.common.QuarkusTestResourceLifecycleManager
import io.quarkus.test.junit.QuarkusTest
import jakarta.inject.Inject
import jakarta.ws.rs.WebApplicationException
import jakarta.ws.rs.client.ClientRequestContext
import jakarta.ws.rs.client.ClientRequestFilter
import jakarta.ws.rs.core.MultivaluedHashMap
import jakarta.ws.rs.core.MultivaluedMap
import org.eclipse.microprofile.rest.client.annotation.RegisterClientHeaders
import org.eclipse.microprofile.rest.client.annotation.ClientHeaderParam
import org.eclipse.microprofile.rest.client.annotation.RegisterProvider
import org.eclipse.microprofile.rest.client.ext.ClientHeadersFactory
import org.eclipse.microprofile.rest.client.inject.RegisterRestClient
import org.eclipse.microprofile.rest.client.inject.RestClient
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertInstanceOf
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.BeforeEach
import org.openapitools.client.apis.PetApi
import org.openapitools.client.models.Pet
import org.openapitools.client.models.Animal
import org.openapitools.client.models.Cat
import org.openapitools.client.models.Dog
import org.openapitools.client.models.Choice
import org.openapitools.client.models.Alternative
import org.openapitools.client.models.FirstChoice
import org.openapitools.client.models.SecondChoice
import java.net.InetSocketAddress
import java.time.Duration
import java.util.concurrent.ConcurrentLinkedQueue

@QuarkusTest
@QuarkusTestResource(PetServer::class)
class PetApiTest @Inject constructor(
    @RestClient private val injectedApi: PetApi?,
    @RestClient private val injectedHeaderApi: HeaderPetApi?,
    private val injectedMapper: ObjectMapper?,
) {
    // Quarkus first constructs a placeholder test instance with null arguments.
    private val api: PetApi get() = checkNotNull(injectedApi)

    @BeforeEach
    fun clearRequests() {
        PetServer.requests.clear()
    }

    @Test
    fun getPet_WithOptionalParameters_UsesJakartaRestAndNullableModels() {
        val id = 1L

        val pet = api.getPet(id, "young", "correlation-id", "session-id").await().atMost(TIMEOUT)
        val request = PetServer.requests.remove()

        assertEquals("Luna", pet.name)
        assertNull(pet.nickname)
        assertNull(pet.age)
        assertEquals(listOf("friendly", null), pet.labels)
        assertEquals("/api/pets/1?filter=young", request.uri)
        assertEquals("correlation-id", request.requestId)
        assertEquals("session=session-id", request.cookie)
    }

    @Test
    fun getPet_WithoutOptionalParameters_OmitsQueryAndHeaders() {
        val id = 1L

        api.getPet(id).await().atMost(TIMEOUT)
        val request = PetServer.requests.remove()

        assertEquals("/api/pets/1", request.uri)
        assertNull(request.requestId)
        assertNull(request.cookie)
    }

    @Test
    fun getPet_WithDerivedClient_PreservesHeaderFactory() {
        val headerApi = checkNotNull(injectedHeaderApi)

        headerApi.getPet(1L).await().atMost(TIMEOUT)
        val request = PetServer.requests.remove()

        assertEquals("generated-client", request.clientHeader)
        assertEquals("test-consumer", request.consumerKey)
        assertEquals("request-filter", request.filterHeader)
    }

    @Test
    fun getAnimal_WithDiscriminator_DecodesSubtypesAndPreservesDecorations() {
        val headerApi = checkNotNull(injectedHeaderApi)
        val mapper = checkNotNull(injectedMapper)

        val cat = headerApi.getAnimal("cat").await().atMost(TIMEOUT)
        val dog = headerApi.getAnimal("dog").await().atMost(TIMEOUT)
        val catJson = mapper.writeValueAsString(cat)
        val dogJson = mapper.writeValueAsString(dog)

        assertInstanceOf(Cat::class.java, cat)
        assertInstanceOf(Dog::class.java, dog)
        assertNull(cat.nickname)
        assertEquals(mapper.readTree(PetServer.CAT), mapper.readTree(catJson))
        assertEquals(mapper.readTree(PetServer.DOG), mapper.readTree(dogJson))
        assertEquals(1, Regex("\"kind\"").findAll(catJson).count())
        assertEquals(1, Regex("\"kind\"").findAll(dogJson).count())
        assertInstanceOf(Cat::class.java, mapper.readValue(catJson, Animal::class.java))
        assertInstanceOf(Dog::class.java, mapper.readValue(dogJson, Animal::class.java))
        repeat(2) {
            val request = PetServer.requests.remove()
            assertEquals("generated-client", request.clientHeader)
            assertEquals("test-consumer", request.consumerKey)
            assertEquals("request-filter", request.filterHeader)
        }
    }

    @Test
    fun getChoice_WithDiscriminatedOneOf_DecodesAndRoundTripsEachSubtype() {
        val mapper = checkNotNull(injectedMapper)

        val first = api.getChoice("first").await().atMost(TIMEOUT)
        val second = api.getChoice("second").await().atMost(TIMEOUT)
        val firstJson = mapper.writeValueAsString(first)
        val secondJson = mapper.writeValueAsString(second)

        assertInstanceOf(FirstChoice::class.java, first)
        assertInstanceOf(SecondChoice::class.java, second)
        assertEquals(mapper.readTree(PetServer.FIRST_CHOICE), mapper.readTree(firstJson))
        assertEquals(mapper.readTree(PetServer.SECOND_CHOICE), mapper.readTree(secondJson))
        assertInstanceOf(FirstChoice::class.java, mapper.readValue(firstJson, Choice::class.java))
        assertInstanceOf(SecondChoice::class.java, mapper.readValue(secondJson, Choice::class.java))
        assertEquals(1, Regex("\"kind\"").findAll(firstJson).count())
        assertEquals(1, Regex("\"kind\"").findAll(secondJson).count())
        repeat(2) { PetServer.requests.remove() }
    }

    @Test
    fun getAnimal_WithUnknownDiscriminator_RejectsResponse() {
        val pending = api.getAnimal("unknown").await()

        val error = assertThrows(RuntimeException::class.java) { pending.atMost(TIMEOUT) }

        val cause = generateSequence<Throwable>(error) { it.cause }.last()
        assertInstanceOf(InvalidTypeIdException::class.java, cause)
        assertEquals("/api/animals/unknown", PetServer.requests.remove().uri)
    }

    @Test
    fun getAlternative_WithDiscriminatedAnyOf_DecodesSharedSubtypes() {
        val mapper = checkNotNull(injectedMapper)

        val first = api.getAlternative("first").await().atMost(TIMEOUT)
        val second = api.getAlternative("second").await().atMost(TIMEOUT)
        val firstJson = mapper.writeValueAsString(first)
        val secondJson = mapper.writeValueAsString(second)

        assertInstanceOf(FirstChoice::class.java, first)
        assertInstanceOf(SecondChoice::class.java, second)
        assertEquals(mapper.readTree(PetServer.FIRST_CHOICE), mapper.readTree(firstJson))
        assertEquals(mapper.readTree(PetServer.SECOND_CHOICE), mapper.readTree(secondJson))
        assertInstanceOf(FirstChoice::class.java, mapper.readValue(firstJson, Alternative::class.java))
        assertInstanceOf(SecondChoice::class.java, mapper.readValue(secondJson, Alternative::class.java))
    }

    @Test
    fun createAnimal_WithConcreteSubtype_PreservesPolymorphicRequestAndResponse() {
        val cat = Cat(name = "Luna", nickname = null, kind = "cat", lives = 9)

        val response = api.createAnimal(cat).await().atMost(TIMEOUT)

        assertEquals(cat, response)
        assertEquals("POST", PetServer.requests.remove().method)
    }

    @Test
    fun getNullablePet_WithJsonNull_CompletesWithNullItem() {
        val client = api

        val pet = client.getNullablePet().await().atMost(TIMEOUT)

        assertNull(pet)
        assertEquals("/api/maybe-pet", PetServer.requests.remove().uri)
    }

    @Test
    fun listPets_WithArrayQuery_RepeatsQueryKey() {
        val tags = listOf("friendly", "young")

        val pets = api.listPets(tags).await().atMost(TIMEOUT)
        val request = PetServer.requests.remove()

        assertEquals("/api/pets?tags=friendly&tags=young", request.uri)
        assertEquals("Luna", pets.single().name)
    }

    @Test
    fun createPet_WithKotlinModel_SerializesRequestBody() {
        val pet = Pet(1L, "Luna", null, listOf("friendly", null))

        val response = api.createPet(pet).await().atMost(TIMEOUT)
        val request = PetServer.requests.remove()

        assertEquals("POST", request.method)
        assertEquals("/api/pets", request.uri)
        assertEquals(pet, response)
    }

    @Test
    fun deletePet_WithNoContent_CompletesWithNullItem() {
        val id = 1L

        val item = api.deletePet(id).await().atMost(TIMEOUT)
        val request = PetServer.requests.remove()

        assertNull(item)
        assertEquals("DELETE", request.method)
    }

    @Test
    fun getPet_WithHttpError_UsesRuntimeExceptionMapper() {
        val pending = api.getPet(404L).await()

        val error = assertThrows(WebApplicationException::class.java) { pending.atMost(TIMEOUT) }

        assertEquals(404, error.response.status)
        PetServer.requests.remove()
    }

    @Test
    fun getPet_WithNullRequiredString_RejectsResponse() {
        val pending = api.getPet(2L).await()

        val error = assertThrows(RuntimeException::class.java) { pending.atMost(TIMEOUT) }

        val cause = generateSequence<Throwable>(error) { it.cause }.last()
        val deserializationError = assertInstanceOf(MismatchedInputException::class.java, cause)
        assertEquals("name", deserializationError.path.last().fieldName)
        assertEquals("/api/pets/2", PetServer.requests.remove().uri)
    }

    @Test
    fun getPet_WithMissingRequiredString_RejectsResponse() {
        val pending = api.getPet(3L).await()

        val error = assertThrows(RuntimeException::class.java) { pending.atMost(TIMEOUT) }

        val cause = generateSequence<Throwable>(error) { it.cause }.last()
        val deserializationError = assertInstanceOf(MismatchedInputException::class.java, cause)
        assertEquals("name", deserializationError.path.last().fieldName)
        assertEquals("/api/pets/3", PetServer.requests.remove().uri)
    }

    @Test
    fun getPet_WithMissingRequiredNullableProperty_RejectsResponse() {
        val pending = api.getPet(4L).await()

        val error = assertThrows(RuntimeException::class.java) { pending.atMost(TIMEOUT) }

        val cause = generateSequence<Throwable>(error) { it.cause }.last()
        val deserializationError = assertInstanceOf(MismatchedInputException::class.java, cause)
        assertEquals("nickname", deserializationError.path.last().fieldName)
        assertEquals("/api/pets/4", PetServer.requests.remove().uri)
    }

    @Test
    fun getPet_WithNullOrMissingRequiredNumber_RejectsResponse() {
        val ids = listOf(5L, 6L)

        ids.forEach { id ->
            val pending = api.getPet(id).await()

            val error = assertThrows(RuntimeException::class.java) { pending.atMost(TIMEOUT) }

            val cause = generateSequence<Throwable>(error) { it.cause }.last()
            val deserializationError = assertInstanceOf(MismatchedInputException::class.java, cause)
            assertEquals("id", deserializationError.path.last().fieldName)
            assertEquals("/api/pets/$id", PetServer.requests.remove().uri)
        }
    }

    companion object {
        private val TIMEOUT = Duration.ofSeconds(5)
    }
}

class PetServer : QuarkusTestResourceLifecycleManager {
    private lateinit var server: HttpServer

    override fun start(): Map<String, String> {
        server = HttpServer.create(InetSocketAddress("localhost", 0), 0)
        server.createContext("/api") { exchange ->
            requests.add(
                Request(
                    exchange.requestMethod,
                    exchange.requestURI.toString(),
                    exchange.requestHeaders.getFirst("X-Request-Id"),
                    exchange.requestHeaders.getFirst("Cookie"),
                    exchange.requestHeaders.getFirst("X-Client-Header"),
                    exchange.requestHeaders.getFirst("Consumer-Key"),
                    exchange.requestHeaders.getFirst("X-Filter"),
                )
            )
            when {
                exchange.requestURI.path == "/api/animals/cat" -> respond(exchange, 200, CAT)
                exchange.requestURI.path == "/api/animals/dog" -> respond(exchange, 200, DOG)
                exchange.requestURI.path == "/api/animals/unknown" ->
                    respond(exchange, 200, """{"kind":"unknown","name":"Unknown","nickname":null}""")
                exchange.requestURI.path == "/api/choices/first" -> respond(exchange, 200, FIRST_CHOICE)
                exchange.requestURI.path == "/api/choices/second" -> respond(exchange, 200, SECOND_CHOICE)
                exchange.requestURI.path == "/api/alternatives/first" -> respond(exchange, 200, FIRST_CHOICE)
                exchange.requestURI.path == "/api/alternatives/second" -> respond(exchange, 200, SECOND_CHOICE)
                exchange.requestURI.path == "/api/maybe-pet" -> respond(exchange, 200, "null")
                exchange.requestMethod == "DELETE" -> respond(exchange, 204, null)
                exchange.requestMethod == "POST" -> respond(exchange, 200, exchange.requestBody.bufferedReader().readText())
                exchange.requestURI.path.endsWith("/404") -> respond(exchange, 404, """{"message":"Not found"}""")
                exchange.requestURI.path.endsWith("/2") -> respond(exchange, 200, """{"id":2,"name":null,"nickname":null,"labels":[]}""")
                exchange.requestURI.path.endsWith("/3") -> respond(exchange, 200, """{"id":3,"nickname":null,"labels":[]}""")
                exchange.requestURI.path.endsWith("/4") -> respond(exchange, 200, """{"id":4,"name":"Luna","labels":[]}""")
                exchange.requestURI.path.endsWith("/5") -> respond(exchange, 200, """{"id":null,"name":"Luna","nickname":null,"labels":[]}""")
                exchange.requestURI.path.endsWith("/6") -> respond(exchange, 200, """{"name":"Luna","nickname":null,"labels":[]}""")
                exchange.requestURI.path == "/api/pets" -> respond(exchange, 200, """[{"id":1,"name":"Luna","nickname":null,"labels":[]}]""")
                else -> respond(exchange, 200, """{"id":1,"name":"Luna","nickname":null,"labels":["friendly",null]}""")
            }
        }
        server.start()
        return mapOf("quarkus.rest-client.pet-service.url" to "http://localhost:${server.address.port}/api")
    }

    override fun stop() {
        server.stop(0)
        requests.clear()
    }

    private fun respond(exchange: HttpExchange, status: Int, body: String?) {
        exchange.responseHeaders.set("Content-Type", "application/json")
        val bytes = body?.toByteArray(Charsets.UTF_8)
        exchange.sendResponseHeaders(status, bytes?.size?.toLong() ?: -1)
        if (bytes != null) {
            exchange.responseBody.use { it.write(bytes) }
        }
        exchange.close()
    }

    data class Request(
        val method: String,
        val uri: String,
        val requestId: String?,
        val cookie: String?,
        val clientHeader: String?,
        val consumerKey: String?,
        val filterHeader: String?,
    )

    companion object {
        const val CAT = """{"kind":"cat","name":"Luna","nickname":null,"lives":9}"""
        const val DOG = """{"kind":"dog","name":"Rex","nickname":null,"breed":"collie"}"""
        const val FIRST_CHOICE = """{"kind":"first","text":"hello"}"""
        const val SECOND_CHOICE = """{"kind":"second","number":2}"""
        val requests = ConcurrentLinkedQueue<Request>()
    }
}

@RegisterRestClient(configKey = "pet-service")
@RegisterClientHeaders(ClientHeaderFactory::class)
@ClientHeaderParam(name = "Consumer-Key", value = ["test-consumer"])
@RegisterProvider(HeaderFilter::class)
interface HeaderPetApi : PetApi

class HeaderFilter : ClientRequestFilter {
    override fun filter(requestContext: ClientRequestContext) {
        requestContext.headers.putSingle("X-Filter", "request-filter")
    }
}

class ClientHeaderFactory : ClientHeadersFactory {
    override fun update(
        incomingHeaders: MultivaluedMap<String, String>,
        outgoingHeaders: MultivaluedMap<String, String>,
    ): MultivaluedMap<String, String> =
        MultivaluedHashMap<String, String>().apply { putSingle("X-Client-Header", "generated-client") }
}
