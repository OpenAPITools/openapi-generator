package io.swagger.client

import io.swagger.client.api._

import com.wordnik.swagger.client._

import java.io.Closeable

class AsyncClient(config: SwaggerConfig) extends Closeable {
  val locator = config.locator
  val name = config.name

  private[this] val client = transportClient

  protected def transportClient: TransportClient = new RestClient(config)

  def close() {
    client.close()
  }
}
