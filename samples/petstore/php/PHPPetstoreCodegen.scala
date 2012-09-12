import com.wordnik.swagger.codegen.BasicPHPGenerator

import com.wordnik.swagger.core._

import java.io.File

object PHPPetstoreCodegen extends BasicPHPGenerator {
  def main(args: Array[String]) = generateClient(args)

  override def destinationDir = "samples/petstore/php"

  override def supportingFiles = List(
    ("Swagger.mustache", destinationDir + File.separator + apiPackage.get, "Swagger.php")
  )
}