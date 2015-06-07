package io.swagger.client

import java.io.Closeable

class SwaggerClient(config: SwaggerConfig) extends Closeable {
  val locator = config.locator
  val name = config.name
  val user = new UserApi(client, config)
  val pet = new PetApi(client, config)
  val store = new StoreApi(client, config)
  private[this] val client = transportClient

  def close() {
    client.close()
  }

  protected def transportClient: TransportClient = new RestClient(config)
}