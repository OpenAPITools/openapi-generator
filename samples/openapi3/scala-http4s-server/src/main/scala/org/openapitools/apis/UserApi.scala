package org.openapitools.apis

import org.openapitools.apis.path._
import org.openapitools.apis.query._

import org.openapitools.models.User
import java.time.ZonedDateTime

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.string.MatchesRegex

import cats.Monad
import cats.syntax.all._

import org.http4s._
import org.http4s.circe._
import org.http4s.server._
import org.http4s.headers._
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.CirceEntityEncoder._

final case class UserApiRoutes[
  F[_]: JsonDecoder: Monad, api_key
](delegate: UserApiDelegate[F, api_key]) extends Http4sDsl[F] {
  object createUser {
    import UserApiDelegate.createUserResponses


    val routeapi_key = AuthedRoutes.of[api_key, F] {
      case (req @ POST -> Root / "user") as auth =>
          delegate.createUser.handle_api_key(auth, req, req.asJsonDecode[User] , responses)

    }

    val responses: createUserResponses[F] = new createUserResponses[F] {
      def resp200(): F[Response[F]] = Ok()
    }
  }
  object createUsersWithArrayInput {
    import UserApiDelegate.createUsersWithArrayInputResponses


    val routeapi_key = AuthedRoutes.of[api_key, F] {
      case (req @ POST -> Root / "user" / "createWithArray") as auth =>
          delegate.createUsersWithArrayInput.handle_api_key(auth, req, req.asJsonDecode[List[User]] , responses)

    }

    val responses: createUsersWithArrayInputResponses[F] = new createUsersWithArrayInputResponses[F] {
      def resp200(): F[Response[F]] = Ok()
    }
  }
  object createUsersWithListInput {
    import UserApiDelegate.createUsersWithListInputResponses


    val routeapi_key = AuthedRoutes.of[api_key, F] {
      case (req @ POST -> Root / "user" / "createWithList") as auth =>
          delegate.createUsersWithListInput.handle_api_key(auth, req, req.asJsonDecode[List[User]] , responses)

    }

    val responses: createUsersWithListInputResponses[F] = new createUsersWithListInputResponses[F] {
      def resp200(): F[Response[F]] = Ok()
    }
  }
  object deleteUser {
    import UserApiDelegate.deleteUserResponses


    val routeapi_key = AuthedRoutes.of[api_key, F] {
      case (req @ DELETE -> Root / "user" / username) as auth =>
        delegate.deleteUser.handle_api_key(auth, req, username, responses)

    }

    val responses: deleteUserResponses[F] = new deleteUserResponses[F] {
      def resp400(): F[Response[F]] = BadRequest()
      def resp404(): F[Response[F]] = NotFound()
    }
  }
  object getUserByName {
    import UserApiDelegate.getUserByNameResponses


    val route = HttpRoutes.of[F] {
      case req @ GET -> Root / "user" / username =>
        delegate.getUserByName.handle(req, username, responses)

    }


    val responses: getUserByNameResponses[F] = new getUserByNameResponses[F] {
      def resp200(value: User): F[Response[F]] = Ok(value)
      def resp200(): F[Response[F]] = Ok()
      def resp400(): F[Response[F]] = BadRequest()
      def resp404(): F[Response[F]] = NotFound()
    }
  }
  object loginUser {
    import UserApiDelegate.loginUserResponses

    object usernameQueryParam extends QueryParamDecoderMatcher[Refined[String, MatchesRegex["^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$"]]]("username")
    object passwordQueryParam extends QueryParamDecoderMatcher[String]("password")

    val route = HttpRoutes.of[F] {
      case req @ GET -> Root / "user" / "login" :? usernameQueryParam(username) +& passwordQueryParam(password) =>
        delegate.loginUser.handle(req, username, password, responses)

    }


    val responses: loginUserResponses[F] = new loginUserResponses[F] {
      def resp200(value: String): F[Response[F]] = Ok(value)
      def resp200(): F[Response[F]] = Ok()
      def resp400(): F[Response[F]] = BadRequest()
    }
  }
  object logoutUser {
    import UserApiDelegate.logoutUserResponses


    val routeapi_key = AuthedRoutes.of[api_key, F] {
      case (req @ GET -> Root / "user" / "logout") as auth =>
        delegate.logoutUser.handle_api_key(auth, req, responses)

    }

    val responses: logoutUserResponses[F] = new logoutUserResponses[F] {
      def resp200(): F[Response[F]] = Ok()
    }
  }
  object updateUser {
    import UserApiDelegate.updateUserResponses


    val routeapi_key = AuthedRoutes.of[api_key, F] {
      case (req @ PUT -> Root / "user" / username) as auth =>
          delegate.updateUser.handle_api_key(auth, req, req.asJsonDecode[User] , username, responses)

    }

    val responses: updateUserResponses[F] = new updateUserResponses[F] {
      def resp400(): F[Response[F]] = BadRequest()
      def resp404(): F[Response[F]] = NotFound()
    }
  }

  val routes =
    getUserByName.route <+>
    loginUser.route
  val routesapi_key =
    createUser.routeapi_key <+>
    createUsersWithArrayInput.routeapi_key <+>
    createUsersWithListInput.routeapi_key <+>
    deleteUser.routeapi_key <+>
    logoutUser.routeapi_key <+>
    updateUser.routeapi_key
}

object UserApiDelegate {
  trait createUserResponses[F[_]] {
    def resp200(): F[Response[F]]
  }

  trait createUsersWithArrayInputResponses[F[_]] {
    def resp200(): F[Response[F]]
  }

  trait createUsersWithListInputResponses[F[_]] {
    def resp200(): F[Response[F]]
  }

  trait deleteUserResponses[F[_]] {
    def resp400(): F[Response[F]]
    def resp404(): F[Response[F]]
  }

  trait getUserByNameResponses[F[_]] {
    def resp200(value: User): F[Response[F]]
    def resp200(): F[Response[F]]
    def resp400(): F[Response[F]]
    def resp404(): F[Response[F]]
  }

  trait loginUserResponses[F[_]] {
    def resp200(value: String): F[Response[F]]
    def resp200(): F[Response[F]]
    def resp400(): F[Response[F]]
  }

  trait logoutUserResponses[F[_]] {
    def resp200(): F[Response[F]]
  }

  trait updateUserResponses[F[_]] {
    def resp400(): F[Response[F]]
    def resp404(): F[Response[F]]
  }

}

trait UserApiDelegate[F[_], api_key] {

  trait createUser {
    import UserApiDelegate.createUserResponses


    def handle_api_key(
      auth: api_key,
      req: Request[F],
      createUser: F[User],
      responses: createUserResponses[F]
    ): F[Response[F]]


  }
  def createUser: createUser


  trait createUsersWithArrayInput {
    import UserApiDelegate.createUsersWithArrayInputResponses


    def handle_api_key(
      auth: api_key,
      req: Request[F],
      createUsersWithArrayInput: F[List[User]],
      responses: createUsersWithArrayInputResponses[F]
    ): F[Response[F]]


  }
  def createUsersWithArrayInput: createUsersWithArrayInput


  trait createUsersWithListInput {
    import UserApiDelegate.createUsersWithListInputResponses


    def handle_api_key(
      auth: api_key,
      req: Request[F],
      createUsersWithListInput: F[List[User]],
      responses: createUsersWithListInputResponses[F]
    ): F[Response[F]]


  }
  def createUsersWithListInput: createUsersWithListInput


  trait deleteUser {
    import UserApiDelegate.deleteUserResponses


    def handle_api_key(
      auth: api_key,
      req: Request[F],
      username: String,
      responses: deleteUserResponses[F]
    ): F[Response[F]]

  }
  def deleteUser: deleteUser


  trait getUserByName {
    import UserApiDelegate.getUserByNameResponses

    def handle(
      req: Request[F],
      username: String,
      responses: getUserByNameResponses[F]
    ): F[Response[F]]

  }
  def getUserByName: getUserByName


  trait loginUser {
    import UserApiDelegate.loginUserResponses

    def handle(
      req: Request[F],
      username: Refined[String, MatchesRegex["^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$"]],
      password: String,
      responses: loginUserResponses[F]
    ): F[Response[F]]

  }
  def loginUser: loginUser


  trait logoutUser {
    import UserApiDelegate.logoutUserResponses


    def handle_api_key(
      auth: api_key,
      req: Request[F],
      responses: logoutUserResponses[F]
    ): F[Response[F]]

  }
  def logoutUser: logoutUser


  trait updateUser {
    import UserApiDelegate.updateUserResponses


    def handle_api_key(
      auth: api_key,
      req: Request[F],
      updateUser: F[User],
      username: String,
      responses: updateUserResponses[F]
    ): F[Response[F]]


  }
  def updateUser: updateUser

}