package com.wordnik.client.apiass PetApi(implicit val swagger: Swagger) extends ScalatraServlet
with FileUploadSupport
with JacksonJsonSupport
with SwaggerSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  protected val applicationDescription: String = "PetApi"
  override protected val applicationName: Option[String] = Some("Pet")

  before() {
    contentType = formats("json")
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }


  val updatePetOperation = (apiOperation[Unit]("updatePet")
    summary "Update an existing pet"
    parameters (


    bodyParam[Pet]("body").description("").optional


    )
    )

  put("/pet", operation(updatePetOperation)) {


    val body = parsedBody.extract[Pet]


    println("body: " + body)

  }


  val addPetOperation = (apiOperation[Unit]("addPet")
    summary "Add a new pet to the store"
    parameters (


    bodyParam[Pet]("body").description("").optional


    )
    )

  post("/pet", operation(addPetOperation)) {


    val body = parsedBody.extract[Pet]


    println("body: " + body)

  }


  val findPetsByStatusOperation = (apiOperation[List[Pet]]("findPetsByStatus")
    summary "Finds Pets by status"
    parameters (
    queryParam[List[String]]("status").description("").optional


    )
    )

  get("/pet/findByStatus", operation(findPetsByStatusOperation)) {


    val statusString = params.getAs[String]("status")
    val status = if ("multi".equals("default")) {
      statusString match {
        case Some(str) => str.split(",")
        case None => List()
      }
    }
    else
      List()











    println("status: " + status)

  }


  val findPetsByTagsOperation = (apiOperation[List[Pet]]("findPetsByTags")
    summary "Finds Pets by tags"
    parameters (
    queryParam[List[String]]("tags").description("").optional


    )
    )

  get("/pet/findByTags", operation(findPetsByTagsOperation)) {


    val tagsString = params.getAs[String]("tags")
    val tags = if ("multi".equals("default")) {
      tagsString match {
        case Some(str) => str.split(",")
        case None => List()
      }
    }
    else
      List()











    println("tags: " + tags)

  }


  val getPetByIdOperation = (apiOperation[Pet]("getPetById")
    summary "Find pet by ID"
    parameters (

    pathParam[Long]("petId").description("")


    )
    )

  get("/pet/{petId}", operation(getPetByIdOperation)) {


    val petId = params.getOrElse("petId", halt(400))










    println("petId: " + petId)

  }


  val updatePetWithFormOperation = (apiOperation[Unit]("updatePetWithForm")
    summary "Updates a pet in the store with form data"
    parameters(

    pathParam[String]("petId").description("")


    ,


    formParam[String]("name").description("").optional

    ,


    formParam[String]("status").description("").optional

    )
    )

  post("/pet/{petId}", operation(updatePetWithFormOperation)) {


    val petId = params.getOrElse("petId", halt(400))










    println("petId: " + petId)










    val name = params.getAs[String]("name")




    println("name: " + name)










    val status = params.getAs[String]("status")




    println("status: " + status)

  }


  val deletePetOperation = (apiOperation[Unit]("deletePet")
    summary "Deletes a pet"
    parameters(

    headerParam[String]("api_key").description("").optional


    ,

    pathParam[Long]("petId").description("")

    )
    )

  delete("/pet/{petId}", operation(deletePetOperation)) {


    val api_key = request.getHeader("api_key")






    println("api_key: " + api_key)




    val petId = params.getOrElse("petId", halt(400))










    println("petId: " + petId)

  }

}