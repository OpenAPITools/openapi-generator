package com.my.company.codegen

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.openapitools.codegen.ClientOptInput
import org.openapitools.codegen.DefaultGenerator
import org.openapitools.codegen.config.CodegenConfigurator

internal class MyclientcodegenGeneratorTest {

    // use this test to launch you code generator in the debugger.
    // this allows you to easily set break points in MyclientcodegenGenerator.
    @Test
    fun launchCodeGenerator() {
        // to understand how the 'openapi-generator-cli' module is using 'CodegenConfigurator', have a look at the 'Generate' class:
        // https://github.com/OpenAPITools/openapi-generator/blob/master/modules/openapi-generator-cli/src/main/java/org/openapitools/codegen/cmd/Generate.java
        val configurator: CodegenConfigurator = CodegenConfigurator()
            .setGeneratorName("myClientCodegen")
            .setInputSpec("https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml") // or from the server
            .setOutputDir("out/myClientCodegen")

        val clientOptInput: ClientOptInput = configurator.toClientOptInput()
        val generator = DefaultGenerator()
        generator.opts(clientOptInput).generate()
    }
}