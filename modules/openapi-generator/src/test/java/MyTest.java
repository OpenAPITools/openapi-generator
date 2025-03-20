import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.util.Map;

public class MyTest {
    @Test
    public void testGeneratedAuthClassesJersey() {

        try {
//            FileUtils.deleteDirectory(new File("C:/Martin/Projekte/openapi-output/"));
        } catch (Exception e) {
            System.out.println("Could not delete directory");
        }

        final CodegenConfigurator configurator = new CodegenConfigurator()

                .setGeneratorName("java")
                .setLibrary("resttemplate")
                .setAdditionalProperties(Map.of(
                        "hideGenerationTimestamp", "true"
                ))
                .setGenerateAliasAsModel(true)

                .setInputSpec("src/test/java/my-test-spec.yaml")
                .setOutputDir("C:/Martin/Projekte/openapi-output/");

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
    }

}
