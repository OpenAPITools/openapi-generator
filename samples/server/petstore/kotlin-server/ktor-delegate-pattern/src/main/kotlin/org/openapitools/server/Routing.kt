package org.openapitools.server

import io.ktor.resources.*
import io.ktor.server.application.*
import io.ktor.server.resources.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.server.routing.get
import kotlinx.serialization.Serializable

fun Application.configureRouting() {
    install(Resources)
    routing {
        get("/") {
            call.respondText("Hello World!")
        }
        get<Articles> { article ->
            // Get all articles ...
            call.respond("List of articles sorted starting from ${article.sort}")
        }
    }
}
@Serializable
@Resource("/articles")
class Articles(val sort: String? = "new")
