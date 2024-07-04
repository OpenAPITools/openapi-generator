package org.openapitools.server.api.verticle
import io.vertx.core.Vertx
import io.vertx.core.AbstractVerticle
import io.vertx.serviceproxy.ServiceBinder

fun main(){
    Vertx.vertx().deployVerticle(StoreApiVerticle())
}

class StoreApiVerticle:AbstractVerticle() {

    override fun start() {
        val instance = (javaClass.classLoader.loadClass("org.openapitools.server.api.verticle.StoreApiImpl").newInstance() as StoreApi)
        instance.init(vertx,config())
        ServiceBinder(vertx)
            .setAddress(StoreApi.address)
            .register(StoreApi::class.java,instance)
    }
}