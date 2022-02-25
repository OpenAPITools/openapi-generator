package org.openapitools.server

import com.codahale.metrics.Slf4jReporter
import io.ktor.application.*
import io.ktor.features.*
import io.ktor.gson.*
import io.ktor.http.*
import io.ktor.locations.*
import io.ktor.metrics.dropwizard.*
import java.util.concurrent.TimeUnit
import io.ktor.routing.*
import io.ktor.util.*
import com.typesafe.config.ConfigFactory
import io.ktor.auth.*
import io.ktor.client.HttpClient
import io.ktor.client.engine.apache.Apache
import io.ktor.config.HoconApplicationConfig
import org.openapitools.server.infrastructure.*
import org.openapitools.server.apis.PetApi
import org.openapitools.server.apis.StoreApi
import org.openapitools.server.apis.UserApi


internal val settings = HoconApplicationConfig(ConfigFactory.defaultApplication(HTTP::class.java.classLoader))

object HTTP {
    val client = HttpClient(Apache)
}

@KtorExperimentalAPI
@KtorExperimentalLocationsAPI
fun Application.main() {
    install(DefaultHeaders)
    install(DropwizardMetrics) {
        val reporter = Slf4jReporter.forRegistry(registry)
            .outputTo(log)
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
    install(Locations) // see https://ktor.io/docs/features-locations.html
    install(Authentication) {
        // "Implement API key auth (api_key) for parameter name 'api_key'."
        apiKeyAuth("api_key") {
            validate { apikeyCredential: ApiKeyCredential ->
                when {
                    apikeyCredential.value == "keyboardcat" -> ApiPrincipal(apikeyCredential)
                    else -> null
                }
            }
        }
        oauth("petstore_auth") {
            client = HttpClient(Apache)
            providerLookup = { ApplicationAuthProviders["petstore_auth"] }
            urlProvider = { _ ->
                // TODO: define a callback url here.
                "/"
            }
        }
    }
    install(Routing) {
        PetApi()
        StoreApi()
        UserApi()
    }

}
