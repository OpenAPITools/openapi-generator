package org.openapitools

import api._
import play.api.inject.{Binding, Module => PlayModule}
import play.api.{Configuration, Environment}

@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2020-02-25T23:20:59.671788+07:00[Asia/Bangkok]")
class Module extends PlayModule {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = Seq(
    bind[PetApi].to[PetApiImpl],
    bind[StoreApi].to[StoreApiImpl],
    bind[UserApi].to[UserApiImpl]
  )
}
