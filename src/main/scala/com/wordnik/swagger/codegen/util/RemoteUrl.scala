package com.wordnik.swagger.codegen.util

import com.wordnik.swagger.codegen.model._

import java.net._
import java.io.InputStream

import scala.io.Source

trait RemoteUrl {
	def urlToString(url: String, authorization: Option [AuthorizationValue]): String = {
		var is: InputStream = null
		try{
			val conn: URLConnection = authorization match {
				case Some(auth: ApiKeyValue) => {
					if(auth.passAs == "header") {
						val connection = new URL(url).openConnection()
						connection.setRequestProperty(auth.keyName, auth.value)
						connection
					}
					else if(auth.passAs == "query") {
						new URL(url + "?%s=%s".format(URLEncoder.encode(auth.keyName, "UTF-8"), URLEncoder.encode(auth.value, "UTF-8"))).openConnection()
					}
					else {
						new URL(url).openConnection()
					}
				}
				case None => new URL(url).openConnection()
			}
			is = conn.getInputStream()
			Source.fromInputStream(is).mkString
		}
		catch {
			case e: javax.net.ssl.SSLProtocolException => {
				println("there is a problem with the target SSL certificate")
				println("**** you may want to run with -Djsse.enableSNIExtension=false\n\n")
				e.printStackTrace
				throw e
			}
			case e: Exception => e.printStackTrace; throw e;
		}
		finally {
			if(is != null) is.close()
		}
	}
}