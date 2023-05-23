package org.openapitools.codegen.kotlin;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.KotlinServerCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.JAXRS_SPEC;

public class KotlinServerCodegenTest {

    @Test(description = "Use jakarta extension")
    public void useJakartaExtension() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(JAXRS_SPEC);
        codegen.additionalProperties().put(KotlinServerCodegen.USE_JAKARTA_EE, true);
        codegen.additionalProperties().put(KotlinServerCodegen.USE_BEANVALIDATION, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/use_jakarta_extension.yaml"))
                        .config(codegen))
                .generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/server/apis/LocationApi.kt"),
                "import jakarta.ws.rs.*"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/server/apis/LocationApi.kt"),
                "import jakarta.ws.rs.core.Response"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/server/apis/LocationApi.kt"),
                "import jakarta.validation.Valid"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/server/models/CreateLocation.kt"),
                "import jakarta.validation.constraints.*"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/server/models/CreateLocation.kt"),
                "import jakarta.validation.Valid"
        );
    }
}