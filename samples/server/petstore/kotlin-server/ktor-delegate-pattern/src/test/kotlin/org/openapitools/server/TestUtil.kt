package org.openapitools.server

import io.ktor.http.HttpStatusCode
import io.ktor.serialization.kotlinx.json.json
import io.ktor.server.auth.Authentication
import io.ktor.server.auth.AuthenticationContext
import io.ktor.server.auth.AuthenticationProvider
import io.ktor.server.plugins.BadRequestException
import io.ktor.server.plugins.contentnegotiation.ContentNegotiation
import io.ktor.server.plugins.di.dependencies
import io.ktor.server.plugins.statuspages.*
import io.ktor.server.resources.Resources
import io.ktor.server.response.respondText
import io.ktor.server.routing.routing
import io.ktor.server.testing.ApplicationTestBuilder
import io.ktor.server.testing.testApplication
import org.openapitools.server.AllApis
import org.openapitools.server.infrastructure.APINotImplementedException
import org.openapitools.server.infrastructure.AppDelegates
import org.openapitools.server.infrastructure.BadParameterException
import org.openapitools.server.infrastructure.delegates


class TestConfig(name: String): AuthenticationProvider.Config(name)

class TestAuthenticationProvider(testConfig: TestConfig) : AuthenticationProvider(testConfig) {
    override suspend fun onAuthenticate(context: AuthenticationContext) {
        context.principal(principal = "test")
    }
}

fun petstoreTestApplication(block: suspend ApplicationTestBuilder.() -> kotlin.Unit) {
    testApplication {
        install(Authentication){
            register(TestAuthenticationProvider(TestConfig(name = "petstore_auth")))
            register(TestAuthenticationProvider(TestConfig(name = "api_key")))
        }
        install(ContentNegotiation) {
            json()
        }
        install(Resources)
        install(StatusPages) {
            exception<Throwable> { call, cause ->
                when (cause) {
                    is BadParameterException -> {
                        call.respondText(
                            status = cause.statusCode, text = mapOf(
                                "message" to (cause.message ?: "Invalid parameter"),
                                "parameter" to cause.parameterName,
                                "validation" to cause.validationType,
                                "expected" to cause.expectedValue,
                                "actual" to cause.actualValue
                            ).filterValues { it != null }.mapValues { it.toString() }.toString()
                        )
                    }

                    is APINotImplementedException -> {
                        call.respondText(status = HttpStatusCode.NotImplemented, text = cause.message ?: "API not implemented")
                    }

                }
            }
        }
        client = createClient {
            install(io.ktor.client.plugins.contentnegotiation.ContentNegotiation) {
                json()
            }
        }

        application {
            dependencies {
                provide(AppDelegates::class)
            }

            val delegatesImpl: AppDelegates by dependencies
            delegates(delegatesImpl)
            routing {
                AllApis()
            }

        }
        block()
    }
}
