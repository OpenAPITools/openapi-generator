package io.swagger.client.api

import io.swagger.client.model.Pet
import java.io.File
import io.swagger.client._
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import collection.mutable

class PetApi(client: TransportClient, config: SwaggerConfig) extends ApiClient(client, config) {

  
  def updatePet(body: Option[Pet] = None
      )(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[Pet]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/pet"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("PUT", path, queryParams.toMap, headerParams.toMap, writer.write(body))
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def addPet(body: Option[Pet] = None
      )(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[Pet]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/pet"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("POST", path, queryParams.toMap, headerParams.toMap, writer.write(body))
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def findPetsByStatus(status: Option[List[String]] = Some(available)
      )(implicit reader: ClientResponseReader[List[Pet]]): Future[List[Pet]] = {
    // create path and map variables
    val path = (addFmt("/pet/findByStatus"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    if(status != null) status.foreach { v => queryParams += "status" -> v.toString }

    

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def findPetsByTags(tags: Option[List[String]] = None
      )(implicit reader: ClientResponseReader[List[Pet]]): Future[List[Pet]] = {
    // create path and map variables
    val path = (addFmt("/pet/findByTags"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    if(tags != null) tags.foreach { v => queryParams += "tags" -> v.toString }

    

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

  
  def updatePetWithForm(petId: String,
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

  
  def deletePet(petId: Long,
      apiKey: Option[String] = None
      )(implicit reader: ClientResponseReader[Unit]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/pet/{petId}")
        replaceAll ("\\{" + "petId" + "\\}",petId.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    headerParams += "api_key" -> apiKey.toString

    val resFuture = client.submit("DELETE", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def uploadFile(petId: Long,
      additionalMetadata: Option[String] = None,
      file: Option[File] = None
      )(implicit reader: ClientResponseReader[Unit]): Future[Unit] = {
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
