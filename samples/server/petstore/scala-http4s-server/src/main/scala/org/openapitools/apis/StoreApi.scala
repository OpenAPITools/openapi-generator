package org.openapitools.apis

import org.openapitools.apis.path._
import org.openapitools.apis.query._

import org.openapitools.models.Order

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.numeric.GreaterEqual
import eu.timepit.refined.numeric.LessEqual

import cats.Monad
import cats.syntax.all._

import org.http4s._
import org.http4s.circe._
import org.http4s.server._
import org.http4s.headers._
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.CirceEntityEncoder._

final case class StoreApiRoutes[
  F[_]: JsonDecoder: Monad, api_key
](delegate: StoreApiDelegate[F, api_key]) extends Http4sDsl[F] {
  object deleteOrder {
    import StoreApiDelegate.deleteOrderResponses


    val route = HttpRoutes.of[F] {
      case req @ DELETE -> Root / "store" / "order" / orderId =>
        delegate.deleteOrder.handle(req, orderId, responses)

    }


    val responses: deleteOrderResponses[F] = new deleteOrderResponses[F] {
      def resp400(): F[Response[F]] = BadRequest()
      def resp404(): F[Response[F]] = NotFound()
    }
  }
  object getInventory {
    import StoreApiDelegate.getInventoryResponses


    val routeapi_key = AuthedRoutes.of[api_key, F] {
      case (req @ GET -> Root / "store" / "inventory") as auth =>
        delegate.getInventory.handle_api_key(auth, req, responses)

    }

    val responses: getInventoryResponses[F] = new getInventoryResponses[F] {
      def resp200(value: Map[String, Int]): F[Response[F]] = Ok(value)
    }
  }
  object getOrderById {
    import StoreApiDelegate.getOrderByIdResponses

    object orderIdVarr extends RefinedVarr[Long, GreaterEqual[1] And LessEqual[5]]

    val route = HttpRoutes.of[F] {
      case req @ GET -> Root / "store" / "order" / orderIdVarr(orderId) =>
        delegate.getOrderById.handle(req, orderId, responses)

    }


    val responses: getOrderByIdResponses[F] = new getOrderByIdResponses[F] {
      def resp200(value: Order): F[Response[F]] = Ok(value)
      def resp200(): F[Response[F]] = Ok()
      def resp400(): F[Response[F]] = BadRequest()
      def resp404(): F[Response[F]] = NotFound()
    }
  }
  object placeOrder {
    import StoreApiDelegate.placeOrderResponses


    val route = HttpRoutes.of[F] {
      case req @ POST -> Root / "store" / "order" =>
          delegate.placeOrder.handle(req, req.asJsonDecode[Order] , responses)

    }


    val responses: placeOrderResponses[F] = new placeOrderResponses[F] {
      def resp200(value: Order): F[Response[F]] = Ok(value)
      def resp200(): F[Response[F]] = Ok()
      def resp400(): F[Response[F]] = BadRequest()
    }
  }

  val routes =
    deleteOrder.route <+>
    getOrderById.route <+>
    placeOrder.route
  val routesapi_key =
    getInventory.routeapi_key
}

object StoreApiDelegate {
  trait deleteOrderResponses[F[_]] {
    def resp400(): F[Response[F]]
    def resp404(): F[Response[F]]
  }

  trait getInventoryResponses[F[_]] {
    def resp200(value: Map[String, Int]): F[Response[F]]
  }

  trait getOrderByIdResponses[F[_]] {
    def resp200(value: Order): F[Response[F]]
    def resp200(): F[Response[F]]
    def resp400(): F[Response[F]]
    def resp404(): F[Response[F]]
  }

  trait placeOrderResponses[F[_]] {
    def resp200(value: Order): F[Response[F]]
    def resp200(): F[Response[F]]
    def resp400(): F[Response[F]]
  }

}

trait StoreApiDelegate[F[_], api_key] {

  trait deleteOrder {
    import StoreApiDelegate.deleteOrderResponses

    def handle(
      req: Request[F],
      orderId: String,
      responses: deleteOrderResponses[F]
    ): F[Response[F]]

  }
  def deleteOrder: deleteOrder


  trait getInventory {
    import StoreApiDelegate.getInventoryResponses


    def handle_api_key(
      auth: api_key,
      req: Request[F],
      responses: getInventoryResponses[F]
    ): F[Response[F]]

  }
  def getInventory: getInventory


  trait getOrderById {
    import StoreApiDelegate.getOrderByIdResponses

    def handle(
      req: Request[F],
      orderId: Refined[Long, GreaterEqual[1] And LessEqual[5]],
      responses: getOrderByIdResponses[F]
    ): F[Response[F]]

  }
  def getOrderById: getOrderById


  trait placeOrder {
    import StoreApiDelegate.placeOrderResponses

    def handle(
      req: Request[F],
      placeOrder: F[Order],
      responses: placeOrderResponses[F]
    ): F[Response[F]]


  }
  def placeOrder: placeOrder

}