package org.openapitools.server

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import org.openapitools.server.api.PetApi
import org.openapitools.server.api.StoreApi
import org.openapitools.server.api.UserApi

import akka.http.scaladsl.server.Directives._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

class Controller(pet: PetApi, store: StoreApi, user: UserApi)(implicit system: ActorSystem, materializer: ActorMaterializer) {

    lazy val routes: Route = pet.route ~ store.route ~ user.route 

    Http().bindAndHandle(routes, "0.0.0.0", 9000)
}