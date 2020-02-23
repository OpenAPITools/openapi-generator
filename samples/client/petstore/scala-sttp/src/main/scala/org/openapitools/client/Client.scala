package org.openapitools.client

import org.openapitools.client.api.PetApi
import org.openapitools.client.core.{ApiKeyValue, SttpSerializer}
import sttp.client.HttpURLConnectionBackend

object Client extends App {

  implicit val serializer = org.json4s.jackson.Serialization

  implicit val sttpSerializer = new SttpSerializer

  val api = new PetApi("https://petstore3.swagger.io/api/v3")
  implicit val backend = HttpURLConnectionBackend()
  implicit val apiKey = ApiKeyValue("api-key")
  //val response = api.getPetById(5)
  val response = api.findPetsByStatus(status = Seq("sold","pending"))

  println(response.toCurl)

  val result = response.send()

  result.body match {
    case Right(r) => println(r)
    case Left(l) => println(l.getMessage)
  }
}
