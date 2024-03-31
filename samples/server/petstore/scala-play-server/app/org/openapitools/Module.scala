package org.openapitools

import api._
import play.api.inject.{Binding, Module => PlayModule}
import play.api.{Configuration, Environment}

@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), comments = "Generator version: 7.5.0-SNAPSHOT")
class Module extends PlayModule {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = Seq(
    bind[PetApi].to[PetApiImpl],
    bind[StoreApi].to[StoreApiImpl],
    bind[UserApi].to[UserApiImpl]
  )
}
