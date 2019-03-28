package api

import model.Order

/**
  * Provides a default implementation for [[StoreApi]].
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-03-26T16:21:58.590+08:00[Asia/Hong_Kong]")
class StoreApiImpl extends StoreApi {
  /**
    * @inheritdoc
    */
  override def deleteOrder(orderId: String): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def getInventory(): Map[String, Int] = {
    // TODO: Implement better logic

    Map.empty[String, Int]
  }

  /**
    * @inheritdoc
    */
  override def getOrderById(orderId: Long): Order = {
    // TODO: Implement better logic

    Order(None, None, None, None, None, None)
  }

  /**
    * @inheritdoc
    */
  override def placeOrder(body: Order): Order = {
    // TODO: Implement better logic

    Order(None, None, None, None, None, None)
  }
}
