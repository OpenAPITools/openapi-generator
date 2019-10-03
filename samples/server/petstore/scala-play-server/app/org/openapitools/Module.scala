package org.openapitools

import api._
import play.api.inject.{Binding, Module => PlayModule}
import play.api.{Configuration, Environment}


class Module extends PlayModule {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = Seq(
    bind[PetApi].to[PetApiImpl],
    bind[StoreApi].to[StoreApiImpl],
    bind[UserApi].to[UserApiImpl]
  )
}
