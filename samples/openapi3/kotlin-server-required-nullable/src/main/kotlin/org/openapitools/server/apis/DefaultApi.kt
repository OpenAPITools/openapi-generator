package org.openapitools.server.apis

import io.javalin.http.Context
import io.javalin.http.bodyAsClass
import io.javalin.http.pathParamAsClass
import io.javalin.http.queryParamAsClass

import org.openapitools.server.models.Pet

class DefaultApi(private val service: DefaultApiService) {
    /**
     * 
     * @param pet  (optional)
     */
    fun addPet(ctx: Context) {
        val result = service.addPet(ctx.bodyAsClass<Pet>(), ctx)
        ctx.status(200).json(result)
    }

}
