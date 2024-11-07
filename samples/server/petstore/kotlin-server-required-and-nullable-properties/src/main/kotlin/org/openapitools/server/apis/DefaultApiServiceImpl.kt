package org.openapitools.server.apis

import org.openapitools.server.models.Pet
import io.javalin.http.Context

class DefaultApiServiceImpl : DefaultApiService {

    override fun addPet(pet: Pet?, ctx: Context): Pet {
        TODO("Implement me")
    }
}
