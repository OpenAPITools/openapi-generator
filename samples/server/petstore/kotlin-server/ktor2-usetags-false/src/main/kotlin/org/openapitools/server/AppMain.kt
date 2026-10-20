package org.openapitools.server

import io.ktor.server.application.*
import io.ktor.serialization.gson.*
import io.ktor.http.*
import io.ktor.server.resources.*
import io.ktor.server.plugins.autohead.*
import io.ktor.server.plugins.compression.*
import io.ktor.server.plugins.contentnegotiation.*
import io.ktor.server.plugins.defaultheaders.*
import io.ktor.server.plugins.hsts.*
import com.codahale.metrics.Slf4jReporter
import io.ktor.server.metrics.dropwizard.*
import java.util.concurrent.TimeUnit
import io.ktor.server.routing.*
import com.typesafe.config.ConfigFactory
import io.ktor.client.HttpClient
import io.ktor.client.engine.apache.Apache
import io.ktor.server.config.HoconApplicationConfig
import io.ktor.server.auth.*
import org.openapitools.server.infrastructure.*
import org.openapitools.server.apis.PetApi
import org.openapitools.server.apis.StoreApi
import org.openapitools.server.apis.UserApi


internal val settings = HoconApplicationConfig(ConfigFactory.defaultApplication(HTTP::class.java.classLoader))

object HTTP {
    val client = HttpClient(Apache)
}

fun Application.main() {
    install(DefaultHeaders)
    install(DropwizardMetrics) {
        val reporter = Slf4jReporter.forRegistry(registry)
            .outputTo(this@main.log)
            .convertRatesTo(TimeUnit.SECONDS)
            .convertDurationsTo(TimeUnit.MILLISECONDS)
            .build()
        reporter.start(10, TimeUnit.SECONDS)
    }
    install(ContentNegotiation) {
        register(ContentType.Application.Json, GsonConverter())
    }
    install(AutoHeadResponse) // see https://ktor.io/docs/autoheadresponse.html
    install(Compression, ApplicationCompressionConfiguration()) // see https://ktor.io/docs/compression.html
    install(HSTS, ApplicationHstsConfiguration()) // see https://ktor.io/docs/hsts.html
    install(Resources)
    install(Authentication) {
        oauth("petstore_auth") {
            client = HttpClient(Apache)
            providerLookup = { applicationAuthProvider(this@main.environment.config) }
            urlProvider = { _ ->
                // TODO: define a callback url here.
                "/"
            }
        }
        // "Implement API key auth (api_key) for parameter name 'api_key'."
        apiKeyAuth("api_key") {
            validate { apikeyCredential: ApiKeyCredential ->
                when {
                    apikeyCredential.value == "keyboardcat" -> ApiPrincipal(apikeyCredential)
                    else -> null
                }
            }
        }
    }
    install(Routing) {
        PetApi()
        StoreApi()
        UserApi()
    }

}
