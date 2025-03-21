package org.openapitools.server.api.api

import misk.inject.KAbstractModule
import misk.web.WebActionModule
import jakarta.inject.Singleton
import org.openapitools.server.api.api.PetApiController
import org.openapitools.server.api.api.StoreApiController
import org.openapitools.server.api.api.UserApiController

@Singleton
class PetStoreModule : KAbstractModule() {
    override fun configure() {
        install(WebActionModule.create<PetApiController>())
        install(WebActionModule.create<StoreApiController>())
        install(WebActionModule.create<UserApiController>())
    }
}