package org.openapitools.codegen.scala;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

public class ScalaPekkoClientCodegenTest {

    @Test
    public void testArrayOfEnumsGeneratesEnumForInnerType() throws Exception {
        File output = Files.createTempDirectory("scalapekko_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("scala-pekko")
                .setInputSpec("src/test/resources/3_0/scala-pekko/array-of-enums.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate().forEach(File::deleteOnExit);

        Path holder = output.toPath().resolve("src/main/scala/org/openapitools/client/model/Holder.scala");
        TestUtils.assertFileContains(holder,
                "colors: Option[Seq[HolderEnums.Colors]] = None,",
                "status: Option[HolderEnums.Status] = None",
                "type Colors = Colors.Value",
                "object Colors extends Enumeration {",
                "val Red = Value(\"red\")",
                "val Green = Value(\"green\")");
        TestUtils.assertFileNotContains(holder, "Seq[Colors].Value", "object Seq[Colors]");

        Path serializers = output.toPath().resolve("src/main/scala/org/openapitools/client/api/EnumsSerializers.scala");
        TestUtils.assertFileContains(serializers, "new EnumNameSerializer(HolderEnums.Colors)");
    }
}
