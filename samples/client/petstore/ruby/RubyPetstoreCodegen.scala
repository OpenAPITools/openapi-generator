import com.wordnik.swagger.codegen.BasicRubyGenerator

import com.wordnik.swagger.core._
import com.wordnik.swagger.codegen.util.ScalaJsonUtil

object RubyPetstoreCodegen extends BasicRubyGenerator {
  def main(args: Array[String]) = generateClient(args)
  
  // to avoid recompiling ...
  override def templateDir = "src/main/resources/ruby"

  // where to write generated code
  override def destinationDir = "samples/client/petstore/ruby"

  override def processModelMap(m: Map[String, AnyRef]): Map[String, AnyRef] = {
  	// println(json.writeValueAsString(m))
  	m
  }
}