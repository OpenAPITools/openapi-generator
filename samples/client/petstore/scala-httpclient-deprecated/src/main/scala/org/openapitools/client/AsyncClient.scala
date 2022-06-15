package org.openapitools.example.invoker

import org.openapitools.example.api._

import com.wordnik.swagger.client._

import java.io.Closeable

class AsyncClient(config: SwaggerConfig) extends Closeable {
  lazy val locator: ServiceLocator = config.locator
  lazy val name: String = config.name

  private[this] val client = transportClient

  protected def transportClient: TransportClient = new RestClient(config)

  def close() {
    client.close()
  }
}
