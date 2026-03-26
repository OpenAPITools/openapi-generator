package org.openapitools

import org.http4s.circe._
import cats.Monad
import cats.syntax.all._
import cats.data.OptionT
import cats.data.Kleisli
import org.http4s._
import org.http4s.server._

import org.openapitools.apis._

final case class API [
  F[_]: JsonDecoder: Monad, petstore_auth, api_key
](
  petstoreAuth: Kleisli[OptionT[F, *], Request[F], petstore_auth],
  apiKey: Kleisli[OptionT[F, *], Request[F], api_key],
)(
  delegateFakeApi: FakeApiDelegate[F],
  delegatePetApi: PetApiDelegate[F, petstore_auth, api_key],
  delegateStoreApi: StoreApiDelegate[F, api_key],
  delegateUserApi: UserApiDelegate[F, api_key],
){
  val apiKeyMiddleware = AuthMiddleware.withFallThrough(apiKey)
  val petstoreAuthMiddleware = AuthMiddleware(petstoreAuth)

  val fakeApiRoutes = new FakeApiRoutes(delegateFakeApi)
  val petApiRoutes = new PetApiRoutes(delegatePetApi)
  val storeApiRoutes = new StoreApiRoutes(delegateStoreApi)
  val userApiRoutes = new UserApiRoutes(delegateUserApi)

  val routes = 
    fakeApiRoutes.routes <+>
      storeApiRoutes.routes <+>
      userApiRoutes.routes
  
  val routesapi_key = apiKeyMiddleware(
    petApiRoutes.routesapi_key <+>
      storeApiRoutes.routesapi_key <+>
      userApiRoutes.routesapi_key
  )
  val routespetstore_auth = petstoreAuthMiddleware(
    petApiRoutes.routespetstore_auth
  )

  val routesAll =
    routes <+>
    routesapi_key <+>
    routespetstore_auth
}
