import com.wordnik.swagger.models._
import com.wordnik.swagger.util.Json
import io.swagger.parser._

import com.wordnik.swagger.codegen.DefaultCodegen

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class CodegenTest extends FlatSpec with Matchers {
  behavior of "Codegen"

  it should "read a file upload param from a 2.0 spec" in {
    val model = new SwaggerParser()
      .read("src/test/resources/2_0/petstore.json")

    val codegen = new DefaultCodegen()
    val path = "/pet/{petId}/upload"
    val p = model.getPaths().get(path).getPost()
    val op = codegen.fromOperation(path, "post", p)

    op.operationId should be ("uploadImage")
    op.httpMethod should be ("POST")
    op.hasConsumes should equal (true)
    op.consumes.size should be(1)
    op.consumes.get(0).get("mediaType") should be ("multipart/form-data")

    op.hasProduces should be (null)

    val allParams = op.allParams
    allParams.size should be (1)

    val formParams = op.formParams
    formParams.size should be (1)

    val file = formParams.get(0)
    file.isFormParam should equal (true)
    file.dataType should be ("file")
    file.required should equal (false)
    file.isFile should equal (true)
    file.hasMore should be (null)
  }

  it should "read formParam values from a 2.0 spec" in {
    val model = new SwaggerParser()
      .read("src/test/resources/2_0/petstore.json")

    val codegen = new DefaultCodegen()
    val path = "/pet/{petId}"
    val p = model.getPaths().get(path).getPost()
    val op = codegen.fromOperation(path, "post", p)

    op.operationId should be ("updatePetWithForm")
    op.httpMethod should be ("POST")
    op.hasConsumes should equal (true)
    op.consumes.size should be(1)
    op.consumes.get(0).get("mediaType") should be ("application/x-www-form-urlencoded")

    op.hasProduces should equal (true)
    op.produces.size should be(2)
    op.produces.get(0).get("mediaType") should be ("application/json")
    op.produces.get(0).get("hasMore") should be ("true")
    op.produces.get(1).get("mediaType") should be ("application/xml")

    val pathParams = op.pathParams
    pathParams.size should be (1)

    val idParam = pathParams.get(0)
    idParam.isPathParam should equal (true)
    idParam.dataType should be ("String")
    idParam.required should equal (true)
    idParam.hasMore should be (null)

    val allParams = op.allParams
    allParams.size should be (3)

    val formParams = op.formParams
    formParams.size should be (2)
    val nameParam = formParams.get(0)
    nameParam.isFormParam should equal (true)
    nameParam.notFile should equal (true)
    nameParam.dataType should be ("String")
    nameParam.required should equal (true)
    nameParam.hasMore should equal (true)

    val statusParam = formParams.get(1)
    statusParam.isFormParam should equal (true)
    statusParam.notFile should equal (true)
    statusParam.dataType should be ("String")
    statusParam.required should equal (true)    
    statusParam.hasMore should be (null)    
  }
}