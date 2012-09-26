import com.wordnik.swagger.codegen.BasicRubyGenerator

import com.wordnik.swagger.core._

object RubyPetstoreCodegen extends BasicRubyGenerator {
  def main(args: Array[String]) = generateClient(args)
  
  // where to write generated code
  override def destinationDir = "samples/client/petstore/ruby"

  // package for models
  override def modelPackage = Some("models")
}