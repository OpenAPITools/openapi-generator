package sbtopenapigenerator.tasks

import org.openapitools.codegen.{CodegenConfigLoader, CodegenType}
import org.openapitools.codegen.meta.Stability
import sbt.{Def, Task}
import scala.collection.JavaConverters._


trait OpenApiGeneratorsTask {

  protected[this] def openApiGeneratorsTask: Def.Initialize[Task[Unit]] = Def.task {
    val generators = CodegenConfigLoader.getAll.asScala
    val types = CodegenType.values()

    val stabilities = Stability.values().filterNot {
      _ == Stability.DEPRECATED
    }

    val out = new StringBuilder

    out ++= "The following generators are available:" + System.lineSeparator()
    for (t <- types) {
      val filteredGenerators = generators.filter(_.getTag == t).sortBy(_.getName)
      if (filteredGenerators.nonEmpty) {
        out ++= s" $t generators:" + System.lineSeparator()
        filteredGenerators.foreach {
          generator =>
            val meta = generator.getGeneratorMetadata
            val stability = meta.getStability
            val include = stabilities.contains(stability)
            if (include) {
              out ++= s"    - ${generator.getName}"
              if (stability != Stability.STABLE) {
                out ++= s" (${stability.value()})"
              }
              out ++= System.lineSeparator()
            }
        }
      }
    }
    out ++= System.lineSeparator()
    out ++= System.lineSeparator()

    println(out)
  }
}
