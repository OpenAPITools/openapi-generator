import com.wordnik.swagger.codegen.BasicJavaGenerator

import com.wordnik.swagger.core._

object JavaPetstoreCodegen extends BasicJavaGenerator {
  def main(args: Array[String]) = generateClient(args)

  override def packageName = "com.wordnik.petstore"

  // location of templates
  override def templateDir = "Java"

  // where to write generated code
  override def destinationDir = "samples/petstore/java/src/main/java"

  // package for models
  override def modelPackage = Some("com.wordnik.petstore.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.petstore.api")

  // supporting classes
  override def supportingFiles =
    List(
      ("apiInvoker.mustache", "samples/petstore/java/src/main/java/com/wordnik/client", "ApiInvoker.java"),
      ("apiException.mustache", "samples/petstore/java/src/main/java/com/wordnik/client", "ApiException.java"),
      ("pom.mustache", "samples/petstore/java", "pom.xml"))
}