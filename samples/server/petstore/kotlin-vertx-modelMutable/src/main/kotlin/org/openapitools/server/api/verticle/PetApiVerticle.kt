package org.openapitools.server.api.verticle
import io.vertx.core.Vertx
import io.vertx.core.AbstractVerticle
import io.vertx.serviceproxy.ServiceBinder

fun main(){
    Vertx.vertx().deployVerticle(PetApiVerticle())
}

class PetApiVerticle:AbstractVerticle() {

    override fun start() {
        val instance = (javaClass.classLoader.loadClass("org.openapitools.server.api.verticle.PetApiImpl").newInstance() as PetApi)
        instance.init(vertx,config())
        ServiceBinder(vertx)
            .setAddress(PetApi.address)
            .register(PetApi::class.java,instance)
    }
}