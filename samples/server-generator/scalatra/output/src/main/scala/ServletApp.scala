package com.wordnik.swagger.sample

import apis._

import com.wordnik.swagger.core.SwaggerSpec

import org.scalatra.{ ScalatraServlet }
import org.scalatra.swagger.{ Swagger, SwaggerBase }

import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.handler.ContextHandlerCollection
import org.eclipse.jetty.servlet.{ ServletContextHandler, ServletHolder }

object ServletApp extends App {
  val server = new Server
  implicit val swagger = new SwaggerApp

  server setGracefulShutdown 5000
  server setSendServerVersion false
  server setSendDateHeader true
  server setStopAtShutdown true

  val connector = new SelectChannelConnector
  connector setPort 8002
  connector setMaxIdleTime 90000

  server addConnector connector

  val apiServlets = new ServletContextHandler(ServletContextHandler.SESSIONS)
  apiServlets setContextPath "/api"

  val handlers = new ContextHandlerCollection
  handlers addHandler apiServlets

  val petServlet = new PetApi
  swagger register ("pet", "/pet", "", petServlet)
  apiServlets addServlet (new ServletHolder(petServlet), "/pet/*")
  apiServlets addServlet (new ServletHolder(new ResourcesApp), "/*")

  val storeServlet = new StoreApi
  swagger register ("store", "/store", "", storeServlet)
  apiServlets addServlet (new ServletHolder(storeServlet), "/pet/*")
  apiServlets addServlet (new ServletHolder(new ResourcesApp), "/*")

  val userServlet = new UserApi
  swagger register ("user", "/user", "", userServlet)
  apiServlets addServlet (new ServletHolder(userServlet), "/pet/*")
  apiServlets addServlet (new ServletHolder(new ResourcesApp), "/*")

  server setHandler handlers
  server start ()
}

class ResourcesApp(implicit val swagger: Swagger) extends ScalatraServlet with SwaggerBase {
  before() {
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }

  protected def buildFullUrl(path: String) = if (path.startsWith("http")) path else {
    "http://%s:%s%s%s".format(
      request.getServerName,
      request.getServerPort,
      request.getContextPath,
      path)
  }
}

class SwaggerApp extends Swagger(SwaggerSpec.version, "1")
