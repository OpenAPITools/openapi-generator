import com.wordnik.swagger.codegen.BasicFlashCodegen

object FlashPetstoreCodegen extends BasicFlashCodegen {
  def main(args: Array[String]) = generateClient(args)

  override def packageName = "com.wordnik.client"

  override def destinationRoot = "samples/client/petstore/flash"

  // where to write generated code
  override def destinationDir = destinationRoot + "/src/main/flex"

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // supporting classes
  override def supportingFiles = baseSupportingFiles ++ List()
}
