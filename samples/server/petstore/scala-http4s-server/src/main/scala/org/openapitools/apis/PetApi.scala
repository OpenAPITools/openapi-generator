package org.openapitools.apis

import org.openapitools.apis.path._
import org.openapitools.apis.query._

import org.openapitools.models.ApiResponse
import java.io.File
import org.openapitools.models.Pet


import cats.Monad
import cats.syntax.all._

import org.http4s._
import org.http4s.circe._
import org.http4s.server._
import org.http4s.headers._
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.CirceEntityEncoder._

final case class PetApiRoutes[
  F[_]: JsonDecoder: Monad, petstore_auth, api_key
](delegate: PetApiDelegate[F, petstore_auth, api_key]) extends Http4sDsl[F] {
  object addPet {
    import PetApiDelegate.addPetResponses


    val routepetstore_auth = AuthedRoutes.of[petstore_auth, F] {
      case (req @ POST -> Root / "pet") as auth =>
      req.contentType match {
        case Some(`Content-Type`(MediaType.application.json, _)) =>
            delegate.addPet.handle_petstore_auth(auth, req, req.asJsonDecode[Pet] , responses)

        case _ =>
          delegate.addPet.handle_petstore_auth(auth, req, responses)

      }
    }

    val responses: addPetResponses[F] = new addPetResponses[F] {
      def resp200(value: Pet): F[Response[F]] = Ok(value)
      def resp200(): F[Response[F]] = Ok()
      def resp405(allow: Allow): F[Response[F]] = MethodNotAllowed(allow)
    }
  }
  object deletePet {
    import PetApiDelegate.deletePetResponses


    val routepetstore_auth = AuthedRoutes.of[petstore_auth, F] {
      case (req @ DELETE -> Root / "pet" / LongVarr(petId)) as auth =>
        delegate.deletePet.handle_petstore_auth(auth, req, petId, responses)

    }

    val responses: deletePetResponses[F] = new deletePetResponses[F] {
      def resp400(): F[Response[F]] = BadRequest()
    }
  }
  object findPetsByStatus {
    import PetApiDelegate.findPetsByStatusResponses

    object statusQueryParam extends QuerySeqParamDecoderMatcher[String]("status")

    val routepetstore_auth = AuthedRoutes.of[petstore_auth, F] {
      case (req @ GET -> Root / "pet" / "findByStatus" :? statusQueryParam(status)) as auth =>
        delegate.findPetsByStatus.handle_petstore_auth(auth, req, status, responses)

    }

    val responses: findPetsByStatusResponses[F] = new findPetsByStatusResponses[F] {
      def resp200(value: List[Pet]): F[Response[F]] = Ok(value)
      def resp200(): F[Response[F]] = Ok()
      def resp400(): F[Response[F]] = BadRequest()
    }
  }
  object findPetsByTags {
    import PetApiDelegate.findPetsByTagsResponses

    object tagsQueryParam extends QuerySeqParamDecoderMatcher[String]("tags")

    val routepetstore_auth = AuthedRoutes.of[petstore_auth, F] {
      case (req @ GET -> Root / "pet" / "findByTags" :? tagsQueryParam(tags)) as auth =>
        delegate.findPetsByTags.handle_petstore_auth(auth, req, tags, responses)

    }

    val responses: findPetsByTagsResponses[F] = new findPetsByTagsResponses[F] {
      def resp200(value: List[Pet]): F[Response[F]] = Ok(value)
      def resp200(): F[Response[F]] = Ok()
      def resp400(): F[Response[F]] = BadRequest()
    }
  }
  object getPetById {
    import PetApiDelegate.getPetByIdResponses


    val routeapi_key = AuthedRoutes.of[api_key, F] {
      case (req @ GET -> Root / "pet" / LongVarr(petId)) as auth =>
        delegate.getPetById.handle_api_key(auth, req, petId, responses)

    }

    val responses: getPetByIdResponses[F] = new getPetByIdResponses[F] {
      def resp200(value: Pet): F[Response[F]] = Ok(value)
      def resp200(): F[Response[F]] = Ok()
      def resp400(): F[Response[F]] = BadRequest()
      def resp404(): F[Response[F]] = NotFound()
    }
  }
  object updatePet {
    import PetApiDelegate.updatePetResponses


    val routepetstore_auth = AuthedRoutes.of[petstore_auth, F] {
      case (req @ PUT -> Root / "pet") as auth =>
      req.contentType match {
        case Some(`Content-Type`(MediaType.application.json, _)) =>
            delegate.updatePet.handle_petstore_auth(auth, req, req.asJsonDecode[Pet] , responses)

        case _ =>
          delegate.updatePet.handle_petstore_auth(auth, req, responses)

      }
    }

    val responses: updatePetResponses[F] = new updatePetResponses[F] {
      def resp200(value: Pet): F[Response[F]] = Ok(value)
      def resp200(): F[Response[F]] = Ok()
      def resp400(): F[Response[F]] = BadRequest()
      def resp404(): F[Response[F]] = NotFound()
      def resp405(allow: Allow): F[Response[F]] = MethodNotAllowed(allow)
    }
  }
  object updatePetWithForm {
    import PetApiDelegate.updatePetWithFormResponses


    val routepetstore_auth = AuthedRoutes.of[petstore_auth, F] {
      case (req @ POST -> Root / "pet" / LongVarr(petId)) as auth =>
        delegate.updatePetWithForm.handle_petstore_auth(auth, req, petId, responses)

    }

    val responses: updatePetWithFormResponses[F] = new updatePetWithFormResponses[F] {
      def resp405(allow: Allow): F[Response[F]] = MethodNotAllowed(allow)
    }
  }
  object uploadFile {
    import PetApiDelegate.uploadFileResponses


    val routepetstore_auth = AuthedRoutes.of[petstore_auth, F] {
      case (req @ POST -> Root / "pet" / LongVarr(petId) / "uploadImage") as auth =>
        delegate.uploadFile.handle_petstore_auth(auth, req, petId, responses)

    }

    val responses: uploadFileResponses[F] = new uploadFileResponses[F] {
      def resp200(value: ApiResponse): F[Response[F]] = Ok(value)
    }
  }

  val routespetstore_auth =
    addPet.routepetstore_auth <+>
    deletePet.routepetstore_auth <+>
    findPetsByStatus.routepetstore_auth <+>
    findPetsByTags.routepetstore_auth <+>
    updatePet.routepetstore_auth <+>
    updatePetWithForm.routepetstore_auth <+>
    uploadFile.routepetstore_auth
  val routesapi_key =
    getPetById.routeapi_key
}

object PetApiDelegate {
  trait addPetResponses[F[_]] {
    def resp200(value: Pet): F[Response[F]]
    def resp200(): F[Response[F]]
    def resp405(allow: Allow): F[Response[F]]
  }

  trait deletePetResponses[F[_]] {
    def resp400(): F[Response[F]]
  }

  trait findPetsByStatusResponses[F[_]] {
    def resp200(value: List[Pet]): F[Response[F]]
    def resp200(): F[Response[F]]
    def resp400(): F[Response[F]]
  }

  trait findPetsByTagsResponses[F[_]] {
    def resp200(value: List[Pet]): F[Response[F]]
    def resp200(): F[Response[F]]
    def resp400(): F[Response[F]]
  }

  trait getPetByIdResponses[F[_]] {
    def resp200(value: Pet): F[Response[F]]
    def resp200(): F[Response[F]]
    def resp400(): F[Response[F]]
    def resp404(): F[Response[F]]
  }

  trait updatePetResponses[F[_]] {
    def resp200(value: Pet): F[Response[F]]
    def resp200(): F[Response[F]]
    def resp400(): F[Response[F]]
    def resp404(): F[Response[F]]
    def resp405(allow: Allow): F[Response[F]]
  }

  trait updatePetWithFormResponses[F[_]] {
    def resp405(allow: Allow): F[Response[F]]
  }

  trait uploadFileResponses[F[_]] {
    def resp200(value: ApiResponse): F[Response[F]]
  }

}

trait PetApiDelegate[F[_], petstore_auth, api_key] {

  trait addPet {
    import PetApiDelegate.addPetResponses


    def handle_petstore_auth(
      auth: petstore_auth,
      req: Request[F],
      addPet: F[Pet],
      responses: addPetResponses[F]
    ): F[Response[F]]



    def handle_petstore_auth(
      auth: petstore_auth,
      req: Request[F],
      responses: addPetResponses[F]
    ): F[Response[F]]

  }
  def addPet: addPet


  trait deletePet {
    import PetApiDelegate.deletePetResponses


    def handle_petstore_auth(
      auth: petstore_auth,
      req: Request[F],
      petId: Long,
      responses: deletePetResponses[F]
    ): F[Response[F]]

  }
  def deletePet: deletePet


  trait findPetsByStatus {
    import PetApiDelegate.findPetsByStatusResponses


    def handle_petstore_auth(
      auth: petstore_auth,
      req: Request[F],
      status: List[String],
      responses: findPetsByStatusResponses[F]
    ): F[Response[F]]

  }
  def findPetsByStatus: findPetsByStatus


  trait findPetsByTags {
    import PetApiDelegate.findPetsByTagsResponses


    def handle_petstore_auth(
      auth: petstore_auth,
      req: Request[F],
      tags: List[String],
      responses: findPetsByTagsResponses[F]
    ): F[Response[F]]

  }
  def findPetsByTags: findPetsByTags


  trait getPetById {
    import PetApiDelegate.getPetByIdResponses


    def handle_api_key(
      auth: api_key,
      req: Request[F],
      petId: Long,
      responses: getPetByIdResponses[F]
    ): F[Response[F]]

  }
  def getPetById: getPetById


  trait updatePet {
    import PetApiDelegate.updatePetResponses


    def handle_petstore_auth(
      auth: petstore_auth,
      req: Request[F],
      updatePet: F[Pet],
      responses: updatePetResponses[F]
    ): F[Response[F]]



    def handle_petstore_auth(
      auth: petstore_auth,
      req: Request[F],
      responses: updatePetResponses[F]
    ): F[Response[F]]

  }
  def updatePet: updatePet


  trait updatePetWithForm {
    import PetApiDelegate.updatePetWithFormResponses


    def handle_petstore_auth(
      auth: petstore_auth,
      req: Request[F],
      petId: Long,
      responses: updatePetWithFormResponses[F]
    ): F[Response[F]]

  }
  def updatePetWithForm: updatePetWithForm


  trait uploadFile {
    import PetApiDelegate.uploadFileResponses


    def handle_petstore_auth(
      auth: petstore_auth,
      req: Request[F],
      petId: Long,
      responses: uploadFileResponses[F]
    ): F[Response[F]]

  }
  def uploadFile: uploadFile

}