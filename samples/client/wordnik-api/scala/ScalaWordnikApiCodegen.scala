import com.wordnik.swagger.codegen.BasicScalaGenerator

import com.wordnik.swagger.core._

object ScalaWordnikApiCodegen extends BasicScalaGenerator {
  def main(args: Array[String]) = generateClient(args)

  def destinationRoot = "samples/client/wordnik-api/scala"

  // where to write generated code
  override def destinationDir = destinationRoot + "/src/main/scala"

  // package for api invoker
  override def invokerPackage = Some("com.wordnik.client.common")

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // supporting classes
  override def supportingFiles = List(
    ("apiInvoker.mustache", destinationDir + "/" + invokerPackage.get.replaceAll("\\.", java.io.File.separator), "ApiInvoker.scala"),
    ("pom.mustache", destinationRoot, "pom.xml"))
}
