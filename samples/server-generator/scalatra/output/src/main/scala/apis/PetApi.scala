package apis

import com.wordnik.client.model.Pet
import com.wordnik.swagger.core.ApiPropertiesReader

import org.scalatra.{ TypedParamSupport, ScalatraServlet }
import org.scalatra.swagger._
import org.json4s._
import org.json4s.JsonDSL._
import org.scalatra.json.{ JValueResult, JacksonJsonSupport }

import scala.collection.JavaConverters._

class PetApi (implicit val swagger: Swagger) extends ScalatraServlet 
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
      parameters(
        pathParam[String]("petId").description(""))
  )
  

  get("/:petId",operation(getPetByIdOperation)) {
    val petId = params.getOrElse("petId", halt(400))
    println(petId)
  }



  val addPetOperation = (apiOperation[Unit]("addPet")
      summary "Add a new pet to the store"
      parameters(
        bodyParam[Pet]("body").description(""))
  )
  

  post("/",operation(addPetOperation)) {
    val body = parsedBody.extract[Pet]
    println(body)
  }



  val updatePetOperation = (apiOperation[Unit]("updatePet")
      summary "Update an existing pet"
      parameters(
        bodyParam[Pet]("body").description(""))
  )
  

  put("/",operation(updatePetOperation)) {
    val body = parsedBody.extract[Pet]
    println(body)
  }



  val findPetsByStatusOperation = (apiOperation[List[Pet]]("findPetsByStatus")
      summary "Finds Pets by status"
      parameters(
        queryParam[String]("status").description("").defaultValue("available"))
  )
  

  get("/findByStatus",operation(findPetsByStatusOperation)) {
    val status = params.getAs[String]("status")
    println(status)
  }



  val findPetsByTagsOperation = (apiOperation[List[Pet]]("findPetsByTags")
      summary "Finds Pets by tags"
      parameters(
        queryParam[String]("tags").description(""))
  )
  

  get("/findByTags",operation(findPetsByTagsOperation)) {
    val tags = params.getAs[String]("tags")
    println(tags)
  }

}
