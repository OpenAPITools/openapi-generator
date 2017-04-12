package io.swagger

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.http.exp.Multipart.FileUpload
import com.twitter.util.Future
import io.finch._, items._
import io.circe.{Encoder, Json}
import io.finch.circe._
import io.circe.generic.semiauto._

import io.swagger.apis._

/**
 * Provides the paths and endpoints for all the API's public service methods.
 */
object endpoint {

  def errorToJson(e: Exception): Json = e match {
      case Error.NotPresent(_) =>
          Json.obj("error" -> Json.fromString("something_not_present"))
      case Error.NotParsed(_, _, _) =>
          Json.obj("error" -> Json.fromString("something_not_parsed"))
      case Error.NotValid(_, _) =>
          Json.obj("error" -> Json.fromString("something_not_valid"))
      case error: PetstoreError =>
          Json.obj("error" -> Json.fromString(error.message))
  }

  implicit val ee: Encoder[Exception] = Encoder.instance {
      case e: Error => errorToJson(e)
      case Errors(nel) => Json.arr(nel.toList.map(errorToJson): _*)
  }

  /**
  * Compiles together all the endpoints relating to public service methods.
  *
  * @return A service that contains all provided endpoints of the API.
  */
  def makeService(da: DataAccessor): Service[Request, Response] = (
          PetApi.endpoints(da)  :+:
          StoreApi.endpoints(da)  :+:
          UserApi.endpoints(da) 
  ).handle({
      case e: PetstoreError => NotFound(e)
  }).toService

}