package io.swagger

import io.finch._
import io.finch.circe._
import io.circe.{Decoder, ObjectEncoder}
import io.circe.generic.auto._
import io.circe.generic.semiauto
import io.circe.generic.semiauto._
import io.circe.java8.time._
import com.twitter.finagle.Http
import com.twitter.finagle.util.LoadService
import com.twitter.util.{Await, Future}


class Server {

  // Loads implementation defined in resources/META-INF/services/io.swagger.DataAccessor
  val db = LoadService[DataAccessor]() match {
    case accessor :: _ => accessor
    case _ => new DataAccessor { }
  }

  val service = endpoint.makeService(db)

  val server = Http.serve(":8080", service) //creates service

  def close(): Future[Unit] = {
    Await.ready(server.close())
  }
}

/**
 * Launches the PetstoreAPI service when the system is ready.
 */
object Server extends Server with App {
  Await.ready(server)
}
