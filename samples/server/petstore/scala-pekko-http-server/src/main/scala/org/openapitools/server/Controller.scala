package org.openapitools.server

import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.server.Route
import org.openapitools.server.api.PetApi
import org.openapitools.server.api.StoreApi
import org.openapitools.server.api.UserApi

import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.ActorMaterializer

class Controller(pet: PetApi, store: StoreApi, user: UserApi)(implicit system: ActorSystem, materializer: ActorMaterializer) {

    lazy val routes: Route = pet.route ~ store.route ~ user.route 

    Http().bindAndHandle(routes, "0.0.0.0", 9000)
}