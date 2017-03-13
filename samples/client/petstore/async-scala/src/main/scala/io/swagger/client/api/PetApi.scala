package io.swagger.client.api

import io.swagger.client.model.ApiResponse
import java.io.File
import io.swagger.client.model.Pet
import com.wordnik.swagger.client._
import scala.concurrent.Future
import collection.mutable

class PetApi(client: TransportClient, config: SwaggerConfig) extends ApiClient(client, config) {

  def addPet(body: Pet)(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[Pet]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/pet"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    if (body == null) throw new Exception("Missing required parameter 'body' when calling PetApi->addPet")

    val resFuture = client.submit("POST", path, queryParams.toMap, headerParams.toMap, writer.write(body))
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def deletePet(petId: Long,
      apiKey: Option[String] = None
      )(implicit reader: ClientResponseReader[Unit]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/pet/{petId}")
        replaceAll ("\\{" + "petId" + "\\}",petId.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    apiKey match {
    case Some(param) => headerParams += "api_key" -> param.toString
    case _ => headerParams
    }

    val resFuture = client.submit("DELETE", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def findPetsByStatus(status: List[String])(implicit reader: ClientResponseReader[List[Pet]]): Future[List[Pet]] = {
    // create path and map variables
    val path = (addFmt("/pet/findByStatus"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    if (status == null) throw new Exception("Missing required parameter 'status' when calling PetApi->findPetsByStatus")
    queryParams += "status" -> status.toString

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def findPetsByTags(tags: List[String])(implicit reader: ClientResponseReader[List[Pet]]): Future[List[Pet]] = {
    // create path and map variables
    val path = (addFmt("/pet/findByTags"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    if (tags == null) throw new Exception("Missing required parameter 'tags' when calling PetApi->findPetsByTags")
    queryParams += "tags" -> tags.toString

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def getPetById(petId: Long)(implicit reader: ClientResponseReader[Pet]): Future[Pet] = {
    // create path and map variables
    val path = (addFmt("/pet/{petId}")
        replaceAll ("\\{" + "petId" + "\\}",petId.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]


    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def updatePet(body: Pet)(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[Pet]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/pet"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    if (body == null) throw new Exception("Missing required parameter 'body' when calling PetApi->updatePet")

    val resFuture = client.submit("PUT", path, queryParams.toMap, headerParams.toMap, writer.write(body))
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def updatePetWithForm(petId: Long,
      name: Option[String] = None,
      status: Option[String] = None
      )(implicit reader: ClientResponseReader[Unit]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/pet/{petId}")
        replaceAll ("\\{" + "petId" + "\\}",petId.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]


    val resFuture = client.submit("POST", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  def uploadFile(petId: Long,
      additionalMetadata: Option[String] = None,
      file: Option[File] = None
      )(implicit reader: ClientResponseReader[ApiResponse]): Future[ApiResponse] = {
    // create path and map variables
    val path = (addFmt("/pet/{petId}/uploadImage")
        replaceAll ("\\{" + "petId" + "\\}",petId.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]


    val resFuture = client.submit("POST", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }


}
