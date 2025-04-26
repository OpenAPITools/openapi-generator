package org.openapitools.codegen.kotlin.misk;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.KotlinMiskServerCodegen;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.assertFileContains;

public class KotlinMiskServerCodegenTest {

    @Test
    public void testDefaultConfiguration() {
        KotlinMiskServerCodegen codegen = new KotlinMiskServerCodegen();
        codegen.processOpts();

        // Test basic properties
        Assert.assertEquals(codegen.getName(), "kotlin-misk");
        Assert.assertEquals(codegen.getHelp(), "Generates a kotlin-misk server.");
        
        // Test package names
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.server.api.api");
        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.server.api.model");
        
        // Test PROTOBUF wire format
        Assert.assertTrue(codegen.getFeatureSet().getWireFormatFeatures().contains(WireFormatFeature.PROTOBUF));
    }

    @Test
    public void testBuildGradleKts() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPI();
        openAPI.info(new Info());
        
        KotlinMiskServerCodegen codegen = new KotlinMiskServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");

        generator.opts(input).generate();

        Path buildGradle = Paths.get(output + "/build.gradle.kts");
        Assert.assertTrue(buildGradle.toFile().exists());
        
        // Verify build.gradle.kts content
        assertFileContains(buildGradle, "id(\"org.jetbrains.kotlin.jvm\")");
        assertFileContains(buildGradle, "implementation(\"com.squareup.misk:misk:");
    }
}