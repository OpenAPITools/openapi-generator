package org.openapitools.server.api.api

import misk.inject.KAbstractModule
import misk.web.WebActionModule
import jakarta.inject.Singleton
import org.openapitools.server.api.api.PetApiAction
import org.openapitools.server.api.api.StoreApiAction
import org.openapitools.server.api.api.UserApiAction

@Singleton
class PetStoreModule : KAbstractModule() {
    override fun configure() {
        install(WebActionModule.create<PetApiAction>())
        install(WebActionModule.create<StoreApiAction>())
        install(WebActionModule.create<UserApiAction>())
    }
}