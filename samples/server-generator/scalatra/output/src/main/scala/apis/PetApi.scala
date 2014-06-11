package apis

import com.wordnik.client.model.Pet
import java.io.File

import org.scalatra.{ TypedParamSupport, ScalatraServlet }
import org.scalatra.swagger._
import org.json4s._
import org.json4s.JsonDSL._
import org.scalatra.json.{ JValueResult, JacksonJsonSupport }
import org.scalatra.servlet.{ FileUploadSupport, MultipartConfig, SizeConstraintExceededException }

import scala.collection.JavaConverters._

class PetApi(implicit val swagger: Swagger) extends ScalatraServlet
    with FileUploadSupport
    with JacksonJsonSupport
    with SwaggerSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  protected val applicationDescription: String = "PetApi"
  override protected val applicationName: Option[String] = Some("pet")

  before() {
    contentType = formats("json")
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }

  val getPetByIdOperation = (apiOperation[Pet]("getPetById")
    summary "Find pet by ID"
    parameters (
      pathParam[Long]("petId").description(""))
  )

  get("/:petId", operation(getPetByIdOperation)) {
    val petId = params.getOrElse("petId", halt(400))
    println("petId: " + petId)
  }

  val deletePetOperation = (apiOperation[Unit]("deletePet")
    summary "Deletes a pet"
    parameters (
      pathParam[String]("petId").description(""))
  )

  delete("/:petId", operation(deletePetOperation)) {
    val petId = params.getOrElse("petId", halt(400))
    println("petId: " + petId)
  }

  val partialUpdateOperation = (apiOperation[List[Pet]]("partialUpdate")
    summary "partial updates to a pet"
    parameters (
      pathParam[String]("petId").description(""), bodyParam[Pet]("body").description(""))
  )

  patch("/:petId", operation(partialUpdateOperation)) {
    val petId = params.getOrElse("petId", halt(400))
    println("petId: " + petId)
    val body = parsedBody.extract[Pet]
    println("body: " + body)
  }

  val updatePetWithFormOperation = (apiOperation[Unit]("updatePetWithForm")
    summary "Updates a pet in the store with form data"
    parameters (
      pathParam[String]("petId").description(""), formParam[String]("name").description(""), formParam[String]("status").description(""))
  )

  post("/:petId", operation(updatePetWithFormOperation)) {
    val petId = params.getOrElse("petId", halt(400))
    println("petId: " + petId)
    val name = params.getAs[String]("name")
    println("name: " + name)
    val status = params.getAs[String]("status")
    println("status: " + status)
  }

  val uploadFileOperation = (apiOperation[Unit]("uploadFile")
    summary "uploads an image"
    parameters (
      formParam[String]("additionalMetadata").description(""),
      bodyParam[File]("body").description("").optional)
  )

  post("/uploadImage", operation(uploadFileOperation)) {
    val additionalMetadata = params.getAs[String]("additionalMetadata")
    println("additionalMetadata: " + additionalMetadata)
    val body = fileParams("body")
    println("body: " + body)
  }

  val addPetOperation = (apiOperation[Unit]("addPet")
    summary "Add a new pet to the store"
    parameters (
      bodyParam[Pet]("body").description(""))
  )

  post("/", operation(addPetOperation)) {
    val body = parsedBody.extract[Pet]
    println("body: " + body)
  }

  val updatePetOperation = (apiOperation[Unit]("updatePet")
    summary "Update an existing pet"
    parameters (
      bodyParam[Pet]("body").description(""))
  )

  put("/", operation(updatePetOperation)) {
    val body = parsedBody.extract[Pet]
    println("body: " + body)
  }

  val findPetsByStatusOperation = (apiOperation[List[Pet]]("findPetsByStatus")
    summary "Finds Pets by status"
    parameters (
      queryParam[String]("status").description("").defaultValue("available"))
  )

  get("/findByStatus", operation(findPetsByStatusOperation)) {
    val status = params.getAs[String]("status")
    println("status: " + status)
  }

  val findPetsByTagsOperation = (apiOperation[List[Pet]]("findPetsByTags")
    summary "Finds Pets by tags"
    parameters (
      queryParam[String]("tags").description(""))
  )

  get("/findByTags", operation(findPetsByTagsOperation)) {
    val tags = params.getAs[String]("tags")
    println("tags: " + tags)
  }

}
