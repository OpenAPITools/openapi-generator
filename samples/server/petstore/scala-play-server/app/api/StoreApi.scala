package api

import model.Order

@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-03-26T16:21:58.590+08:00[Asia/Hong_Kong]")
trait StoreApi {
  /**
    * Delete purchase order by ID
    * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    * @param orderId ID of the order that needs to be deleted
    */
  def deleteOrder(orderId: String): Unit

  /**
    * Returns pet inventories by status
    * Returns a map of status codes to quantities
    */
  def getInventory(): Map[String, Int]

  /**
    * Find purchase order by ID
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    * @param orderId ID of pet that needs to be fetched
    */
  def getOrderById(orderId: Long): Order

  /**
    * Place an order for a pet
    * @param body order placed for purchasing the pet
    */
  def placeOrder(body: Order): Order
}
