package com.wordnik.client.api

class StoreApi(implicit val swagger: Swagger) extends ScalatraServlet
with FileUploadSupport
with JacksonJsonSupport
with SwaggerSupport {
  override protected val applicationName: Option[String] = Some("Store")
  val getInventoryOperation = (apiOperation[Map[String, Int]]("getInventory")
    summary "Returns pet inventories by status"
    parameters(
    )
    )
  val placeOrderOperation = (apiOperation[Order]("placeOrder")
    summary "Place an order for a pet"
    parameters (


    bodyParam[Order]("body").description("").optional


    )
    )

  before() {
    contentType = formats("json")
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }
  val getOrderByIdOperation = (apiOperation[Order]("getOrderById")
    summary "Find purchase order by ID"
    parameters (

    pathParam[String]("orderId").description("")


    )
    )

  get("/store/inventory", operation(getInventoryOperation)) {

  }
  val deleteOrderOperation = (apiOperation[Unit]("deleteOrder")
    summary "Delete purchase order by ID"
    parameters (

    pathParam[String]("orderId").description("")


    )
    )

  post("/store/order", operation(placeOrderOperation)) {


    val body = parsedBody.extract[Order]


    println("body: " + body)

  }
  protected implicit val jsonFormats: Formats = DefaultFormats

  get("/store/order/{orderId}", operation(getOrderByIdOperation)) {


    val orderId = params.getOrElse("orderId", halt(400))










    println("orderId: " + orderId)

  }
  protected val applicationDescription: String = "StoreApi"

  delete("/store/order/{orderId}", operation(deleteOrderOperation)) {


    val orderId = params.getOrElse("orderId", halt(400))










    println("orderId: " + orderId)

  }

}