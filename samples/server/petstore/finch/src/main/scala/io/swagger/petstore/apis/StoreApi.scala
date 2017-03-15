package io.swagger.petstore.apis

import java.io._
import java.util.Date
import io.swagger.petstore._
import io.swagger.petstore.models._
import io.swagger.petstore.models.Order
import io.finch.circe._
import io.circe.generic.semiauto._
import com.twitter.concurrent.AsyncStream
import com.twitter.finagle.Service
import com.twitter.finagle.Http
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.http.exp.Multipart.{FileUpload, InMemoryFileUpload, OnDiskFileUpload}
import com.twitter.util.Future
import com.twitter.io.Buf
import io.finch._, items._
import java.io.File

object StoreApi {
    /**
    * Compiles all service endpoints.
    * @return Bundled compilation of all service endpoints.
    */
    def endpoints(da: DataAccessor) =
            deleteOrder(da) :+:
            getInventory(da) :+:
            getOrderById(da) :+:
            placeOrder(da)

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def deleteOrder(da: DataAccessor): Endpoint[Unit] =
        delete("store" :: "order" :: string ) { (orderId: String) => 
                da.Store_deleteOrder(orderId)
                NoContent[Unit]
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Map[String, Int]
        */
        private def getInventory(da: DataAccessor): Endpoint[Map[String, Int]] =
        get("store" :: "inventory" ) { 
                Ok(da.Store_getInventory())
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Order
        */
        private def getOrderById(da: DataAccessor): Endpoint[Order] =
        get("store" :: "order" :: long ) { (orderId: Long) => 
                Ok(da.Store_getOrderById(orderId))
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Order
        */
        private def placeOrder(da: DataAccessor): Endpoint[Order] =
        post("store" :: "order"  :: jsonBody[Order]) { (body: Order) => 
                Ok(da.Store_placeOrder(body))
        } handle {
          case e: Exception => BadRequest(e)
        }


    implicit private def fileUploadToFile(fileUpload: FileUpload) : File = {
        fileUpload match {
            case upload: InMemoryFileUpload =>
                bytesToFile(Buf.ByteArray.Owned.extract(upload.content))
            case upload: OnDiskFileUpload =>
                upload.content
            case _ => null
        }
    }

    private def bytesToFile(input: Array[Byte]): java.io.File = {
        val file = File.createTempFile("tmpStoreApi", null)
        val output = new FileOutputStream(file)
        output.write(input)
        file
    }

    // This assists in params(string) application (which must be Seq[A] in parameter list) when the param is used as a List[A] elsewhere.
    implicit def seqList[A](input: Seq[A]): List[A] = input.toList
}
