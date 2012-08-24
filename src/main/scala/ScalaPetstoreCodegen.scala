import com.wordnik.swagger.codegen.BasicScalaGenerator

import com.wordnik.swagger.core._

object ScalaPetstoreCodegen extends BasicScalaGenerator {
  def main(args: Array[String]) = generateClient(args)

  override def packageName = "com.wordnik.petstore"
    
  // where to write generated code
  override def destinationDir = "samples/scala/src/main/scala"

  // package for models
  override def modelPackage = Some("com.wordnik.petstore.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.petstore.api")
  
  // supporting classes
  override def supportingFiles = List(
    ("apiInvoker.mustache", "samples/scala/src/main/scala/com/wordnik/client", "ApiInvoker.scala"),
    ("pom.mustache", "samples/scala", "pom.xml")
  )
}