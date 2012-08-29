package apis

import com.wordnik.swagger.core.ApiPropertiesReader
import com.wordnik.client.model.Pet
import org.scalatra.ScalatraServlet
import org.scalatra.swagger._

import scala.collection.JavaConverters._

class PetApi (implicit val swagger: Swagger) extends ScalatraServlet with SwaggerBase with SwaggerSupport {
  protected def buildFullUrl(path: String) = "http://petstore.swagger.wordnik.com/api/%s" format path

  get("/:petId",
    summary("Find pet by ID"),
    nickname("getPetById"),
    responseClass("Pet"),
    endpoint("/pet.{format}/{petId}"),
    notes("Returns a pet based on ID"),
    parameters(
      Parameter("petId", "ID of pet that needs to be fetched",
        dataType = DataType.String,
        paramType = ParamType.Path)
      
      )) {
  }

  post("/",
    summary("Add a new pet to the store"),
    nickname("addPet"),
    responseClass("void"),
    endpoint("/pet.{format}"),
    notes(""),
    parameters(
      Parameter("body", "Pet object that needs to be added to the store",
        dataType = DataType("Pet"),
        paramType = ParamType.Body)
      
      )) {
  }

  put("/",
    summary("Update an existing pet"),
    nickname("updatePet"),
    responseClass("void"),
    endpoint("/pet.{format}"),
    notes(""),
    parameters(
      Parameter("body", "Pet object that needs to be updated in the store",
        dataType = DataType("Pet"),
        paramType = ParamType.Body)
      
      )) {
  }

  get("/findByStatus",
    summary("Finds Pets by status"),
    nickname("findPetsByStatus"),
    responseClass("List[Pet]"),
    endpoint("/pet.{format}/findByStatus"),
    notes("Multiple status values can be provided with comma seperated strings"),
    parameters(
      Parameter("status", "Status values that need to be considered for filter",
        paramType = ParamType.Query,
        required = true,
        allowMultiple = true,
        allowableValues = AllowableValues("LIST[available,pending,sold]"),defaultValue = Some("available"),
        dataType = DataType("String"))
      
      )) {
  }

  get("/findByTags",
    summary("Finds Pets by tags"),
    nickname("findPetsByTags"),
    responseClass("List[Pet]"),
    endpoint("/pet.{format}/findByTags"),
    notes("Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing."),
    parameters(
      Parameter("tags", "Tags to filter by",
        paramType = ParamType.Query,
        required = true,
        allowMultiple = true,
        defaultValue = None,
        dataType = DataType("String"))
      
      )) {
  }

  }
