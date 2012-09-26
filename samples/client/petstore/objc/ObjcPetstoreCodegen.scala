import com.wordnik.swagger.codegen.BasicObjcGenerator

import com.wordnik.swagger.core._

object ObjcPetstoreCodegen extends BasicObjcGenerator {
  def main(args: Array[String]) = generateClient(args)

  // where to write generated code
  override def destinationDir = "samples/client/petstore/objc/client"

  // supporting classes
  override def supportingFiles =
    List(
      ("NIKSwaggerObject.h", destinationDir, "NIKSwaggerObject.h"),
      ("NIKSwaggerObject.m", destinationDir, "NIKSwaggerObject.m"),
      ("NIKApiInvoker.h", destinationDir, "NIKApiInvoker.h"),
      ("NIKApiInvoker.m", destinationDir, "NIKApiInvoker.m"),
      ("NIKDate.h", destinationDir, "NIKDate.h"),
      ("NIKDate.m", destinationDir, "NIKDate.m"))
}
