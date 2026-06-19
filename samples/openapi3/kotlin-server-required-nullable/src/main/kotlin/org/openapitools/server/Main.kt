package org.openapitools.server

import io.javalin.Javalin
import io.javalin.apibuilder.ApiBuilder.*

import org.openapitools.server.apis.DefaultApi
import org.openapitools.server.apis.DefaultApiServiceImpl

fun main() {
    val defaultApi = DefaultApi(DefaultApiServiceImpl())

    val app = Javalin
        .create { config ->
            config.router.apiBuilder {
                path("/pet") { post(defaultApi::addPet) }

            }
        }

    app.start()
}
