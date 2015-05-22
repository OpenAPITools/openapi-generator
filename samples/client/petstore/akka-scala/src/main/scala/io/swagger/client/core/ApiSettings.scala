package io.swagger.client.core

import java.util.concurrent.TimeUnit

import akka.actor.{ExtendedActorSystem, Extension, ExtensionKey}
import com.typesafe.config.Config
import io.swagger.client.core.ApiInvoker.CustomStatusCode
import spray.http.HttpHeaders.RawHeader

import scala.collection.JavaConversions._
import scala.concurrent.duration.FiniteDuration

class ApiSettings(config: Config) extends Extension {
  def this(system: ExtendedActorSystem) = this(system.settings.config)

  private def cfg = config.getConfig("io.swagger.client.apiRequest")

  val alwaysTrustCertificates = cfg.getBoolean("trust-certificates")
  val defaultHeaders = cfg.getConfig("default-headers").entrySet.toList.map(c => RawHeader(c.getKey, c.getValue.render))
  val connectionTimeout = FiniteDuration(cfg.getDuration("connection-timeout", TimeUnit.MILLISECONDS), TimeUnit.MILLISECONDS)
  val compressionEnabled = cfg.getBoolean("compression.enabled")
  val compressionSizeThreshold = cfg.getBytes("compression.size-threshold").toInt
  val customCodes = cfg.getConfigList("custom-codes").toList.map { c => CustomStatusCode(
    c.getInt("code"),
    c.getString("reason"),
    c.getBoolean("success"))
  }


}

object ApiSettings extends ExtensionKey[ApiSettings]
