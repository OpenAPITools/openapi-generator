import com.wordnik.swagger.codegen.BasicPythonGenerator

import com.wordnik.swagger.core._

import java.io.File

object PythonPetstoreCodegen extends BasicPythonGenerator {
  def main(args: Array[String]) = generateClient(args)

  override def destinationDir = "samples/petstore/python"

  override def supportingFiles = List(
    ("__init__.mustache", destinationDir, "__init__.py"),
    ("swagger.mustache", destinationDir + File.separator + apiPackage.get, "swagger.py"),
    ("__init__.mustache", destinationDir + File.separator + modelPackage.get, "__init__.py"))
}
