package org.openapitools.server.api.verticle

import org.openapitools.server.api.model.ModelApiResponse
import org.openapitools.server.api.model.Pet
import io.vertx.core.Vertx
import io.vertx.core.json.JsonObject
import io.vertx.core.json.JsonArray
import com.github.wooyme.openapi.Response
import io.vertx.ext.web.api.OperationRequest
import io.vertx.kotlin.ext.web.api.contract.openapi3.OpenAPI3RouterFactory
import io.vertx.serviceproxy.ServiceBinder
import io.vertx.ext.web.handler.CookieHandler
import io.vertx.ext.web.handler.SessionHandler
import io.vertx.ext.web.sstore.LocalSessionStore
import java.util.List
import java.util.Map


interface PetApi  {
    fun init(vertx:Vertx,config:JsonObject)
    /* addPet
     * Add a new pet to the store */
    suspend fun addPet(body:Pet?,context:OperationRequest):Response<Void>
    /* deletePet
     * Deletes a pet */
    suspend fun deletePet(petId:kotlin.Long?,apiKey:kotlin.String?,context:OperationRequest):Response<Void>
    /* findPetsByStatus
     * Finds Pets by status */
    suspend fun findPetsByStatus(status:kotlin.Array<kotlin.String>?,context:OperationRequest):Response<kotlin.Array<Pet>>
    /* findPetsByTags
     * Finds Pets by tags */
    suspend fun findPetsByTags(tags:kotlin.Array<kotlin.String>?,context:OperationRequest):Response<kotlin.Array<Pet>>
    /* getPetById
     * Find pet by ID */
    suspend fun getPetById(petId:kotlin.Long?,context:OperationRequest):Response<Pet>
    /* updatePet
     * Update an existing pet */
    suspend fun updatePet(body:Pet?,context:OperationRequest):Response<Void>
    /* updatePetWithForm
     * Updates a pet in the store with form data */
    suspend fun updatePetWithForm(petId:kotlin.Long?,name:kotlin.String?,status:kotlin.String?,context:OperationRequest):Response<Void>
    /* uploadFile
     * uploads an image */
    suspend fun uploadFile(petId:kotlin.Long?,additionalMetadata:kotlin.String?,file:kotlin.collections.List<java.io.File>?,context:OperationRequest):Response<ModelApiResponse>
    companion object {
        const val address = "PetApi-service"
        suspend fun createRouterFactory(vertx: Vertx,path:String): io.vertx.ext.web.api.contract.openapi3.OpenAPI3RouterFactory {
            val routerFactory = OpenAPI3RouterFactory.createAwait(vertx,path)
            routerFactory.addGlobalHandler(CookieHandler.create())
            routerFactory.addGlobalHandler(SessionHandler.create(LocalSessionStore.create(vertx)))
            routerFactory.setExtraOperationContextPayloadMapper{
                JsonObject().put("files",JsonArray(it.fileUploads().map { it.uploadedFileName() }))
            }
            val opf = routerFactory::class.java.getDeclaredField("operations")
            opf.isAccessible = true
            val operations = opf.get(routerFactory) as Map<String, Any>
            for (m in PetApi::class.java.methods) {
                val methodName = m.name
                val op = operations[methodName]
                if (op != null) {
                    val method = op::class.java.getDeclaredMethod("mountRouteToService",String::class.java,String::class.java)
                    method.isAccessible = true
                    method.invoke(op,address,methodName)
                }
            }
            routerFactory.mountServiceInterface(PetApi::class.java, address)
            return routerFactory
        }
    }
}
