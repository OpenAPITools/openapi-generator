package org.openapitools.client

import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpServer
import io.quarkus.test.common.QuarkusTestResource
import io.quarkus.test.common.QuarkusTestResourceLifecycleManager
import io.quarkus.test.junit.QuarkusTest
import jakarta.inject.Inject
import jakarta.ws.rs.WebApplicationException
import jakarta.ws.rs.core.MultivaluedHashMap
import jakarta.ws.rs.core.MultivaluedMap
import org.eclipse.microprofile.rest.client.annotation.RegisterClientHeaders
import org.eclipse.microprofile.rest.client.ext.ClientHeadersFactory
import org.eclipse.microprofile.rest.client.inject.RegisterRestClient
import org.eclipse.microprofile.rest.client.inject.RestClient
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test
import org.openapitools.client.apis.PetApi
import org.openapitools.client.models.Pet
import java.net.InetSocketAddress
import java.time.Duration
import java.util.concurrent.ConcurrentLinkedQueue

@QuarkusTest
@QuarkusTestResource(PetServer::class)
class PetApiTest @Inject constructor(
    @RestClient private val injectedApi: PetApi?,
    @RestClient private val injectedHeaderApi: HeaderPetApi?,
) {
    // Quarkus first constructs a placeholder test instance with null arguments.
    private val api: PetApi get() = checkNotNull(injectedApi)

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

        assertThrows(RuntimeException::class.java) { pending.atMost(TIMEOUT) }

        PetServer.requests.remove()
    }

    @Test
    fun getPet_WithMissingRequiredString_RejectsResponse() {
        val pending = api.getPet(3L).await()

        assertThrows(RuntimeException::class.java) { pending.atMost(TIMEOUT) }

        PetServer.requests.remove()
    }

    companion object {
        private val TIMEOUT = Duration.ofSeconds(5)
    }
}

class PetServer : QuarkusTestResourceLifecycleManager {
    private lateinit var server: HttpServer

    override fun start(): Map<String, String> {
        server = HttpServer.create(InetSocketAddress("localhost", 0), 0)
        server.createContext("/api/pets") { exchange ->
            requests.add(
                Request(
                    exchange.requestMethod,
                    exchange.requestURI.toString(),
                    exchange.requestHeaders.getFirst("X-Request-Id"),
                    exchange.requestHeaders.getFirst("Cookie"),
                    exchange.requestHeaders.getFirst("X-Client-Header"),
                )
            )
            when {
                exchange.requestMethod == "DELETE" -> respond(exchange, 204, null)
                exchange.requestMethod == "POST" -> respond(exchange, 200, exchange.requestBody.bufferedReader().readText())
                exchange.requestURI.path.endsWith("/404") -> respond(exchange, 404, """{"message":"Not found"}""")
                exchange.requestURI.path.endsWith("/2") -> respond(exchange, 200, """{"id":2,"name":null,"nickname":null,"labels":[]}""")
                exchange.requestURI.path.endsWith("/3") -> respond(exchange, 200, """{"id":3,"nickname":null,"labels":[]}""")
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

    data class Request(val method: String, val uri: String, val requestId: String?, val cookie: String?, val clientHeader: String?)

    companion object {
        val requests = ConcurrentLinkedQueue<Request>()
    }
}

@RegisterRestClient(configKey = "pet-service")
@RegisterClientHeaders(ClientHeaderFactory::class)
interface HeaderPetApi : PetApi

class ClientHeaderFactory : ClientHeadersFactory {
    override fun update(
        incomingHeaders: MultivaluedMap<String, String>,
        outgoingHeaders: MultivaluedMap<String, String>,
    ): MultivaluedMap<String, String> =
        MultivaluedHashMap<String, String>().apply { putSingle("X-Client-Header", "generated-client") }
}
