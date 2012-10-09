package apis

import com.wordnik.client.model.Pet
import com.wordnik.swagger.core.ApiPropertiesReader

import org.scalatra.{ TypedParamSupport, ScalatraServlet }
import org.scalatra.swagger._
import org.json4s._
import org.json4s.JsonDSL._
import org.scalatra.json.{JValueResult, NativeJsonSupport}

import scala.collection.JavaConverters._

class PetApi (implicit val swagger: Swagger) extends ScalatraServlet 
      with TypedParamSupport 
      with NativeJsonSupport 
      with JValueResult 
      with SwaggerSupport  {
  protected implicit val jsonFormats: Formats = DefaultFormats

  protected val applicationDescription: String = "PetApi"
  override protected val applicationName: Option[String] = Some("pet")

  def swaggerToModel(cls: Class[_]) = {
    val docObj = ApiPropertiesReader.read(cls)
    val name = docObj.getName
    val fields = for (field <- docObj.getFields.asScala.filter(d => d.paramType != null))
      yield (field.name -> ModelField(field.name, field.notes, DataType(field.paramType)))

    Model(name, name, fields.toMap)
  }

  before() {
    contentType = formats("json")
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }

  get("/:petId",
    summary("Find pet by ID"),
    nickname("getPetById"),
    responseClass("Pet"),
    endpoint("{petId}"),
    notes("Returns a pet based on ID"),
    parameters(
      Parameter(name = "petId", 
        description = "ID of pet that needs to be fetched",
        dataType = DataType.String,
        defaultValue = None,
        paramType = ParamType.Path)
      )) {

    // do something
  }

  post("/",
    summary("Add a new pet to the store"),
    nickname("addPet"),
    responseClass("void"),
    endpoint(""),
    notes(""),
    parameters(
      Parameter(name = "body",
        description = "Pet object that needs to be added to the store",
        dataType = DataType("Pet"),
        paramType = ParamType.Body)
      )) {

    // do something
  }

  put("/",
    summary("Update an existing pet"),
    nickname("updatePet"),
    responseClass("void"),
    endpoint(""),
    notes(""),
    parameters(
      Parameter(name = "body",
        description = "Pet object that needs to be updated in the store",
        dataType = DataType("Pet"),
        paramType = ParamType.Body)
      )) {

    // do something
  }

  get("/findByStatus",
    summary("Finds Pets by status"),
    nickname("findPetsByStatus"),
    responseClass("List[Pet]"),
    endpoint("findByStatus"),
    notes("Multiple status values can be provided with comma seperated strings"),
    parameters(
      Parameter(name = "status", 
        description = "Status values that need to be considered for filter",
        paramType = ParamType.Query,
        required = true,
        allowMultiple = true,
        allowableValues = AllowableValues("available","pending","sold"),defaultValue = Some("available"),
        dataType = DataType("String"))
      )) {

    // do something
  }

  get("/findByTags",
    summary("Finds Pets by tags"),
    nickname("findPetsByTags"),
    responseClass("List[Pet]"),
    endpoint("findByTags"),
    notes("Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing."),
    parameters(
      Parameter(name = "tags", 
        description = "Tags to filter by",
        paramType = ParamType.Query,
        required = true,
        allowMultiple = true,
        defaultValue = None,
        dataType = DataType("String"))
      )) {

    // do something
  }
}
