package org.openapitools.server.api.verticle

import org.openapitools.server.api.model.User
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


interface UserApi  {
    fun init(vertx:Vertx,config:JsonObject)
    /* createUser
     * Create user */
    suspend fun createUser(body:User?,context:OperationRequest):Response<Void>
    /* createUsersWithArrayInput
     * Creates list of users with given input array */
    suspend fun createUsersWithArrayInput(body:kotlin.Array<User>?,context:OperationRequest):Response<Void>
    /* createUsersWithListInput
     * Creates list of users with given input array */
    suspend fun createUsersWithListInput(body:kotlin.Array<User>?,context:OperationRequest):Response<Void>
    /* deleteUser
     * Delete user */
    suspend fun deleteUser(username:kotlin.String?,context:OperationRequest):Response<Void>
    /* getUserByName
     * Get user by user name */
    suspend fun getUserByName(username:kotlin.String?,context:OperationRequest):Response<User>
    /* loginUser
     * Logs user into the system */
    suspend fun loginUser(username:kotlin.String?,password:kotlin.String?,context:OperationRequest):Response<kotlin.String>
    /* logoutUser
     * Logs out current logged in user session */
    suspend fun logoutUser(context:OperationRequest):Response<Void>
    /* updateUser
     * Updated user */
    suspend fun updateUser(username:kotlin.String?,body:User?,context:OperationRequest):Response<Void>
    companion object {
        const val address = "UserApi-service"
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
            for (m in UserApi::class.java.methods) {
                val methodName = m.name
                val op = operations[methodName]
                if (op != null) {
                    val method = op::class.java.getDeclaredMethod("mountRouteToService",String::class.java,String::class.java)
                    method.isAccessible = true
                    method.invoke(op,address,methodName)
                }
            }
            routerFactory.mountServiceInterface(UserApi::class.java, address)
            return routerFactory
        }
    }
}
