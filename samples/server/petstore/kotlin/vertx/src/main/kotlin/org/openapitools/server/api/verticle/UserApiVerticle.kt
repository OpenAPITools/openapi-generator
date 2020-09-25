package org.openapitools.server.api.verticle
import io.vertx.core.Vertx
import io.vertx.core.AbstractVerticle
import io.vertx.serviceproxy.ServiceBinder

fun main(){
    Vertx.vertx().deployVerticle(UserApiVerticle())
}

class UserApiVerticle:AbstractVerticle() {

    override fun start() {
        val instance = (javaClass.classLoader.loadClass("org.openapitools.server.api.verticle.UserApiImpl").newInstance() as UserApi)
        instance.init(vertx,config())
        ServiceBinder(vertx)
            .setAddress(UserApi.address)
            .register(UserApi::class.java,instance)
    }
}