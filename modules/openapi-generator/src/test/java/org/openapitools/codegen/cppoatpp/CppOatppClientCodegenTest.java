package org.openapitools.codegen.cppoatpp;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;

public class CppOatppClientCodegenTest {

    @Test
    public void enumValuesBecomeValidFunctionNames() throws Exception {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-oatpp-client")
                .setInputSpec("src/test/resources/3_0/cpp-oatpp-client/enum-names.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        TestUtils.assertFileContains(Paths.get(output.getAbsolutePath(), "model", "EnumFoo.hpp"),
                "static EnumFoo Enum_Foo_Foo(void) { return \"Enum-Foo-Foo\"; }",
                "static EnumFoo available(void) { return \"available\"; }",
                "static EnumFoo _2nd_place(void) { return \"2nd place\"; }",
                "static EnumFoo r_new(void) { return \"new\"; }");
    }
}
