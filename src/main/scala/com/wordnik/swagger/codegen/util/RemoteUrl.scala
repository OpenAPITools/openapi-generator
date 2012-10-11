package com.wordnik.swagger.codegen.util

import java.net.URL
import java.io.InputStream

import scala.io.Source

trait RemoteUrl {
	def urlToString(url: String): String = {
		var is: InputStream = null
		try{
			val conn = new URL(url).openConnection()
			is = conn.getInputStream()
			Source.fromInputStream(is).mkString
		}
		finally {
			if(is != null) is.close()
		}
	}
}