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
import org.openapitools.server.api.model.ApiResponse
import org.openapitools.server.api.model.Pet

class PetApiVertxProxyHandler(private val vertx: Vertx, private val service: PetApi, topLevel: Boolean, private val timeoutSeconds: Long) : ProxyHandler() {
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
        
                "addPet" -> {
                    val params = context.params
                    val bodyParam = ApiHandlerUtils.searchJsonObjectInJson(params,"body")
                    if (bodyParam == null) {
                        throw IllegalArgumentException("body is required")
                    }
                    val body = Gson().fromJson(bodyParam.encode(), Pet::class.java)
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.addPet(body,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "deletePet" -> {
                    val params = context.params
                    val petId = ApiHandlerUtils.searchLongInJson(params,"petId")
                    if(petId == null){
                        throw IllegalArgumentException("petId is required")
                    }
                    val apiKey = ApiHandlerUtils.searchStringInJson(params,"api_key")
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.deletePet(petId,apiKey,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "findPetsByStatus" -> {
                    val params = context.params
                    val statusParam = ApiHandlerUtils.searchJsonArrayInJson(params,"status")
                    if(statusParam == null){
                         throw IllegalArgumentException("status is required")
                    }
                    val status:kotlin.Array<kotlin.String> = Gson().fromJson(statusParam.encode()
                            , object : TypeToken<kotlin.collections.List<kotlin.String>>(){}.type)
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.findPetsByStatus(status,context)
                        val payload = JsonArray(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "findPetsByTags" -> {
                    val params = context.params
                    val tagsParam = ApiHandlerUtils.searchJsonArrayInJson(params,"tags")
                    if(tagsParam == null){
                         throw IllegalArgumentException("tags is required")
                    }
                    val tags:kotlin.Array<kotlin.String> = Gson().fromJson(tagsParam.encode()
                            , object : TypeToken<kotlin.collections.List<kotlin.String>>(){}.type)
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.findPetsByTags(tags,context)
                        val payload = JsonArray(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "getPetById" -> {
                    val params = context.params
                    val petId = ApiHandlerUtils.searchLongInJson(params,"petId")
                    if(petId == null){
                        throw IllegalArgumentException("petId is required")
                    }
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.getPetById(petId,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "updatePet" -> {
                    val params = context.params
                    val bodyParam = ApiHandlerUtils.searchJsonObjectInJson(params,"body")
                    if (bodyParam == null) {
                        throw IllegalArgumentException("body is required")
                    }
                    val body = Gson().fromJson(bodyParam.encode(), Pet::class.java)
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.updatePet(body,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "updatePetWithForm" -> {
                    val params = context.params
                    val petId = ApiHandlerUtils.searchLongInJson(params,"petId")
                    if(petId == null){
                        throw IllegalArgumentException("petId is required")
                    }
                    val name = ApiHandlerUtils.searchStringInJson(params,"name")
                    val status = ApiHandlerUtils.searchStringInJson(params,"status")
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.updatePetWithForm(petId,name,status,context)
                        val payload = JsonObject(Json.encode(result.payload)).toBuffer()
                        val res = OperationResponse(result.statusCode,result.statusMessage,payload,result.headers)
                        msg.reply(res.toJson())
                    }.invokeOnCompletion{
                        it?.let{ throw it }
                    }
                }
        
                "uploadFile" -> {
                    val params = context.params
                    val petId = ApiHandlerUtils.searchLongInJson(params,"petId")
                    if(petId == null){
                        throw IllegalArgumentException("petId is required")
                    }
                    val additionalMetadata = ApiHandlerUtils.searchStringInJson(params,"additionalMetadata")
                    val fileParam = context.extra.getJsonArray("files")
                    val file = fileParam?.map{ java.io.File(it as String) }
                    GlobalScope.launch(vertx.dispatcher()){
                        val result = service.uploadFile(petId,additionalMetadata,file,context)
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
