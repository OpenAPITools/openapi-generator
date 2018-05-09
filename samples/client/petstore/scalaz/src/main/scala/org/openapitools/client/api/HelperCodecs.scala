package org.openapitools.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s._
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._

import org.joda.time.DateTime

object HelperCodecs {
  implicit def returnTypeDecoder[A: EncodeJson]: EntityEncoder[List[A]] = jsonEncoderOf[List[A]]
}
