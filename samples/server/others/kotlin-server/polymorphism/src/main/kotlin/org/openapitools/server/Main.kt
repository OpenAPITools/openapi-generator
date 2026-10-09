package org.openapitools.server

import io.javalin.Javalin
import io.javalin.apibuilder.ApiBuilder.*


fun main() {

    val app = Javalin
        .create { config ->
            config.router.apiBuilder {
            }
        }

    app.start()
}
