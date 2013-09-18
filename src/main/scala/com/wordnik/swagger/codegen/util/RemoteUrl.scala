package com.wordnik.swagger.codegen.util

import com.wordnik.swagger.model._

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
						new URL(url + "?%s=%s".format(URLEncoder.encode(auth.keyName), URLEncoder.encode(auth.value))).openConnection()
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
		finally {
			if(is != null) is.close()
		}
	}
}