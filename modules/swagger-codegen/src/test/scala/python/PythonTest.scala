package python

import io.swagger.codegen.languages.PythonClientCodegen
import io.swagger.parser.SwaggerParser
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PythonTest extends FlatSpec with Matchers {
  it should "convert a python model with dots" in {
    val swagger = new SwaggerParser()
      .read("src/test/resources/2_0/v1beta3.json")

    val codegen = new PythonClientCodegen()
    val simpleName = codegen.fromModel("v1beta3.Binding", swagger.getDefinitions().get("v1beta3.Binding"))
    simpleName.name should be("v1beta3.Binding")
    simpleName.classname should be("V1beta3Binding")
    simpleName.classVarName should be("v1beta3_binding")

    val compoundName = codegen.fromModel("v1beta3.ComponentStatus", swagger.getDefinitions().get("v1beta3.ComponentStatus"))
    compoundName.name should be("v1beta3.ComponentStatus")
    compoundName.classname should be("V1beta3ComponentStatus")
    compoundName.classVarName should be("v1beta3_component_status")

    val path = "/api/v1beta3/namespaces/{namespaces}/bindings"
    val operation = swagger.getPaths().get(path).getPost()
    val codegenOperation = codegen.fromOperation(path, "get", operation, swagger.getDefinitions())
    codegenOperation.returnType should be("V1beta3Binding")
    codegenOperation.returnBaseType should be("V1beta3Binding")
  }
}
