import com.wordnik.swagger.codegen.BasicPython3Generator

import com.wordnik.swagger.core._

import java.io.File

object Python3PetstoreCodegen extends BasicPython3Generator {
  def main(args: Array[String]) = generateClient(args)

  override def destinationDir = "samples/client/petstore/python3"

  override def supportingFiles = List(
    ("__init__.mustache", destinationDir, "__init__.py"),
    ("swagger.mustache", destinationDir + File.separator + apiPackage.get, "swagger.py"),
    ("__init__.mustache", destinationDir + File.separator + modelPackage.get, "__init__.py"))
}
