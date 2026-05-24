package org.openapitools.server

import io.ktor.server.routing.*
import org.openapitools.server.apis.PetApi
import org.openapitools.server.apis.StoreApi
import org.openapitools.server.apis.UserApi



fun Route.AllApis() {
    PetApi()
    StoreApi()
    UserApi()
}
