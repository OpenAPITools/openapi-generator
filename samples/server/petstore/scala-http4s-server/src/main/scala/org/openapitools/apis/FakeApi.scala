package org.openapitools.apis

import org.openapitools.apis.path._
import org.openapitools.apis.query._



import cats.Monad
import cats.syntax.all._

import org.http4s._
import org.http4s.circe._
import org.http4s.server._
import org.http4s.headers._
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.CirceEntityEncoder._

final case class FakeApiRoutes[
  F[_]: JsonDecoder: Monad
](delegate: FakeApiDelegate[F]) extends Http4sDsl[F] {
  object reservedWords {
    import FakeApiDelegate.reservedWordsResponses

    object varQueryParam extends OptionalQueryParamDecoderMatcher[String]("var")

    val route = HttpRoutes.of[F] {
      case req @ GET -> Root / "fake" / "user" / _type :? varQueryParam(_var) =>
        delegate.reservedWords.handle(req, _type, _var, responses)

    }


    val responses: reservedWordsResponses[F] = new reservedWordsResponses[F] {
      def resp200(): F[Response[F]] = Ok()
      def resp400(): F[Response[F]] = BadRequest()
      def resp404(): F[Response[F]] = NotFound()
    }
  }

  val routes =
    reservedWords.route
}

object FakeApiDelegate {
  trait reservedWordsResponses[F[_]] {
    def resp200(): F[Response[F]]
    def resp400(): F[Response[F]]
    def resp404(): F[Response[F]]
  }

}

trait FakeApiDelegate[F[_]] {

  trait reservedWords {
    import FakeApiDelegate.reservedWordsResponses

    def handle(
      req: Request[F],
      `type`: String,
      `var`: Option[String],
      responses: reservedWordsResponses[F]
    ): F[Response[F]]

  }
  def reservedWords: reservedWords

}