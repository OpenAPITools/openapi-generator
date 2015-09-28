import io.swagger.codegen.DefaultCodegen
import io.swagger.models.properties.Property
import io.swagger.parser._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class CodegenTest extends FlatSpec with Matchers {
  behavior of "Codegen"

  it should "read a file upload param from a 2.0 spec" in {
    val model = new SwaggerParser()
      .read("src/test/resources/2_0/petstore.json")

    val codegen = new DefaultCodegen()
    val path = "/pet/{petId}/uploadImage"
    val p = model.getPaths().get(path).getPost()
    val op = codegen.fromOperation(path, "post", p, model.getDefinitions())

    op.operationId should be("uploadFile")
    op.httpMethod should be("POST")
    op.hasConsumes should equal(true)
    op.consumes.size should be(1)
    op.consumes.get(0).get("mediaType") should be("multipart/form-data")

    op.hasProduces should equal(true)
    val allParams = op.allParams
    allParams.size should be(3)

    val formParams = op.formParams
    formParams.size should be(2)

    val file = formParams.get(1)
    file.isFormParam should equal(true)
    file.dataType should be("file")
    file.required should equal(null)
    file.isFile should equal(true)
    file.hasMore should be(null)
  }

  it should "read formParam values from a 2.0 spec" in {
    val model = new SwaggerParser()
      .read("src/test/resources/2_0/petstore.json")

    val codegen = new DefaultCodegen()
    val path = "/pet/{petId}"
    val p = model.getPaths().get(path).getPost()
    val op = codegen.fromOperation(path, "post", p, model.getDefinitions())

    op.operationId should be("updatePetWithForm")
    op.httpMethod should be("POST")
    op.hasConsumes should equal(true)
    op.consumes.size should be(1)
    op.consumes.get(0).get("mediaType") should be("application/x-www-form-urlencoded")

    op.hasProduces should equal(true)
    op.produces.size should be(2)
    op.produces.get(0).get("mediaType") should be("application/json")
    op.produces.get(0).get("hasMore") should be("true")
    op.produces.get(1).get("mediaType") should be("application/xml")

    val pathParams = op.pathParams
    pathParams.size should be(1)

    val idParam = pathParams.get(0)
    idParam.isPathParam should equal(true)
    idParam.dataType should be("String")
    idParam.required should equal(true)
    idParam.hasMore should be(null)

    val allParams = op.allParams
    allParams.size should be(3)

    val formParams = op.formParams
    formParams.size should be(2)
    val nameParam = formParams.get(0)
    nameParam.isFormParam should equal(true)
    nameParam.notFile should equal(true)
    nameParam.dataType should be("String")
    nameParam.required should equal(null)
    nameParam.hasMore should equal(true)

    val statusParam = formParams.get(1)
    statusParam.isFormParam should equal(true)
    statusParam.notFile should equal(true)
    statusParam.dataType should be("String")
    statusParam.required should equal(null)
    statusParam.hasMore should be(null)
  }

  it should "handle required parameters from a 2.0 spec as required when figuring out Swagger types" in {
    val model = new SwaggerParser()
      .read("src/test/resources/2_0/requiredTest.json")

    val codegen = new DefaultCodegen() {
      override def getSwaggerType(p: Property) = Option(p) match {
        case Some(property) if !property.getRequired =>
          "Optional<" + super.getSwaggerType(p) + ">"
        case other => super.getSwaggerType(p)
      }
    }
    val path = "/tests/requiredParams"
    val p = model.getPaths().get(path).getGet()
    val op = codegen.fromOperation(path, "get", p, model.getDefinitions)

    val formParams = op.formParams
    formParams.size should be(2)
    val requiredParam = formParams.get(0)
    requiredParam.dataType should be("Long")

    val optionalParam = formParams.get(1)
    optionalParam.dataType should be("Optional<string>")

    op.returnType should be("Long")
  }

  it should "select main response from a 2.0 spec using the lowest 2XX code" in {
    val model = new SwaggerParser()
      .read("src/test/resources/2_0/responseSelectionTest.json")

    val codegen = new DefaultCodegen()

    val path = "/tests/withTwoHundredAndDefault"
    val p = model.getPaths().get(path).getGet()
    val op = codegen.fromOperation(path, "get", p, model.getDefinitions())
    op.returnType should be("String")

  }

  it should "select main response from a 2.0 spec using the default keyword when no 2XX code" in {
    val model = new SwaggerParser()
      .read("src/test/resources/2_0/responseSelectionTest.json")

    val codegen = new DefaultCodegen()

    val path = "/tests/withoutTwoHundredButDefault"
    val p = model.getPaths().get(path).getGet()
    val op = codegen.fromOperation(path, "get", p, model.getDefinitions())
    op.returnType should be("String")
  }

  it should "return byte array when response format is byte" in {
      val model = new SwaggerParser()
        .read("src/test/resources/2_0/binaryDataTest.json")
      System.err.println("model is " + model);
      val codegen = new DefaultCodegen()

      val path = "/tests/binaryResponse"
      val p = model.getPaths().get(path).getPost()
      val op = codegen.fromOperation(path, "post", p, model.getDefinitions())
      op.returnType should be("byte[]")
      op.bodyParam.dataType should be ("byte[]")
      op.bodyParam.isBinary should equal (true);
      op.responses.get(0).isBinary should equal(true);
    }
}