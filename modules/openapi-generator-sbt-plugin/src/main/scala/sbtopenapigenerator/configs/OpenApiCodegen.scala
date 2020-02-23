package sbtopenapigenerator.configs

import sbt.librarymanagement.Configuration

trait OpenApiCodegen {

  protected lazy val OpenApiCodegen: Configuration = Configuration.of(
    id = "OpenApiCodegen",
    name = "OpenApi Generator codegen plugin configuration"
  )

}
