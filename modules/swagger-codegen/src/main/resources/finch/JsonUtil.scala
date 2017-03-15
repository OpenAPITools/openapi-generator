package json

import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.JsonGenerator.Feature
import com.fasterxml.jackson.databind._

object JsonUtil {
  val mapper = new ObjectMapper()
  mapper.registerModule(new DefaultScalaModule())
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)
}