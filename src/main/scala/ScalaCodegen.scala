import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object ScalaCodegen extends ScalaCodegen {
  def main(args: Array[String]) = generateClient(args)
}

class ScalaCodegen extends BasicGenerator {
  // location of templates
  override def templateDir = "scala"
}
