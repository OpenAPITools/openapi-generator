package org.openapitools.server.api.verticle

import io.vertx.core.Vertx
import io.vertx.core.eventbus.Message
import io.vertx.core.json.JsonObject
import io.vertx.ext.web.api.OperationRequest
import io.vertx.ext.web.api.OperationResponse
import io.vertx.ext.web.api.generator.ApiHandlerUtils
import io.vertx.serviceproxy.ProxyHandler
import io.vertx.serviceproxy.ServiceException
import io.vertx.serviceproxy.ServiceExceptionMessageCodec
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import io.vertx.kotlin.coroutines.dispatcher
import io.vertx.core.json.Json
import io.vertx.core.json.JsonArray
import com.google.gson.reflect.TypeToken
import com.google.gson.Gson
import org.openapitools.server.api.model.User

class UserApiVertxProxyHandler(private val vertx: Vertx, private val service: UserApi, topLevel: Boolean, private val timeoutSeconds: Long) : ProxyHandler() {
    private val timerID: Long
    private var lastAccessed: Long = 0
    init {
        try {
            this.vertx.eventBus().registerDefaultCodec(ServiceException::class.java,
            ServiceExceptionMessageCodec())
        } catch (ex: IllegalStateException) {}

        if (timeoutSeconds != (-1).toLong() && !topLevel) {
            var period = timeoutSeconds * 1000 / 2
            if (period > 10000) {
                period = 10000
            }
            this.timerID = vertx.setPeriodic(period) { this.checkTimedOut(it) }
        } else {
            this.timerID = -1
        }
        accessed()
    }
    private fun checkTimedOut(id: Long) {
        val now = System.nanoTime()
        if (now - lastAccessed > timeoutSeconds * 1000000000) {
            close()
        }
    }

    override fun close() {
        if (timerID != (-1).toLong()) {
            vertx.cancelTimer(timerID)
        }
        super.close()
    }

    private fun accessed() {
        this.lastAccessed = System.nanoTime()
    }
    override fun handle(msg: Message<JsonObject>) {
        try {
            val json = msg.body()
            val action = msg.headers().get("action") ?: throw IllegalStateException("action not specified")
            accessed()
            val contextSerialized = json.getJsonObject("context") ?: throw IllegalStateException("Received action $action without OperationRequest \"context\"")
            val context = OperationRequest(contextSerialized)
            when (action) {
        
                "createUser" -> {
                    val params = context.params
                    val bodyParam = ApiHandlerUtils.searchJsonObjectInJson(params,"body")
                    if (bodyParam == null) {
                        throw IllegalArgumentException("body is required")
                    }
                    val body = Gson().fromJson(bodyParam.encode(), User::class.java)
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.createUser(body,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "createUsersWithArrayInput" -> {
                    val params = context.params
                    val bodyParam = ApiHandlerUtils.searchJsonArrayInJson(params,"body")
                    if(bodyParam == null){
                         throw IllegalArgumentException("body is required")
                    }
                    val body:kotlin.Array<User> = Gson().fromJson(bodyParam.encode()
                            , object : TypeToken<kotlin.collections.List<User>>(){}.type)
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.createUsersWithArrayInput(body,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "createUsersWithListInput" -> {
                    val params = context.params
                    val bodyParam = ApiHandlerUtils.searchJsonArrayInJson(params,"body")
                    if(bodyParam == null){
                         throw IllegalArgumentException("body is required")
                    }
                    val body:kotlin.Array<User> = Gson().fromJson(bodyParam.encode()
                            , object : TypeToken<kotlin.collections.List<User>>(){}.type)
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.createUsersWithListInput(body,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "deleteUser" -> {
                    val params = context.params
                    val username = ApiHandlerUtils.searchStringInJson(params,"username")
                    if(username == null){
                        throw IllegalArgumentException("username is required")
                    }
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.deleteUser(username,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "getUserByName" -> {
                    val params = context.params
                    val username = ApiHandlerUtils.searchStringInJson(params,"username")
                    if(username == null){
                        throw IllegalArgumentException("username is required")
                    }
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.getUserByName(username,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "loginUser" -> {
                    val params = context.params
                    val username = ApiHandlerUtils.searchStringInJson(params,"username")
                    if(username == null){
                        throw IllegalArgumentException("username is required")
                    }
                    val password = ApiHandlerUtils.searchStringInJson(params,"password")
                    if(password == null){
                        throw IllegalArgumentException("password is required")
                    }
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.loginUser(username,password,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "logoutUser" -> {
                }
        
                "updateUser" -> {
                    val params = context.params
                    val username = ApiHandlerUtils.searchStringInJson(params,"username")
                    if(username == null){
                        throw IllegalArgumentException("username is required")
                    }
                    val bodyParam = ApiHandlerUtils.searchJsonObjectInJson(params,"body")
                    if (bodyParam == null) {
                        throw IllegalArgumentException("body is required")
                    }
                    val body = Gson().fromJson(bodyParam.encode(), User::class.java)
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.updateUser(username,body,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
            }
        }catch (t: Throwable) {
            msg.reply(ServiceException(500, t.message))
            throw t
        }
    }
}
