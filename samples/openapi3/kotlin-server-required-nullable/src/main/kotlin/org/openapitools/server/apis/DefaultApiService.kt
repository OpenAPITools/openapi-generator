package org.openapitools.server.apis

import org.openapitools.server.models.Pet
import io.javalin.http.Context

interface DefaultApiService {

    /**
     * POST /pet
     *
     * @param pet  (optional)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return Successful operation (status code 200)
     * @see DefaultApi#addPet
     */
    fun addPet(pet: Pet?, ctx: Context): Pet
}
