package org.openapitools.codegen.kotlin;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.KotlinSpringServerCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.testng.Assert.assertTrue;

public class KotlinSpringServerCodegenTest {

    @Test
    public void gradleWrapperIsGenerated() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();

        codegen.setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(
                new ClientOptInput().openAPI(TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml"))
                        .config(codegen)
        ).generate();
        String outputPath = output.getAbsolutePath();
        Path gradleWrapperProperties = Paths.get(outputPath + "/gradle/wrapper/gradle-wrapper.properties");
        Path gradleWrapperJar = Paths.get(outputPath + "/gradle/wrapper/gradle-wrapper.jar");
        Path gradleWrapper = Paths.get(outputPath + "/gradlew");
        Path gradleWrapperBat = Paths.get(outputPath + "/gradlew.bat");
        TestUtils.assertFileExists(gradleWrapperProperties);
        TestUtils.assertFileExists(gradleWrapper);
        TestUtils.assertFileExists(gradleWrapperBat);
        //Different because file is not a text file
        assertTrue(Files.exists(gradleWrapperJar));

        //Spring Cloud
        File outputCloud = Files.createTempDirectory("testCloud").toFile().getCanonicalFile();
        outputCloud.deleteOnExit();
        codegen.setLibrary(KotlinSpringServerCodegen.SPRING_CLOUD_LIBRARY);
        codegen.setOutputDir(outputCloud.getAbsolutePath());
        new DefaultGenerator().opts(
                new ClientOptInput().openAPI(TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml"))
                        .config(codegen)
        ).generate();

        String outputPathCloud = outputCloud.getAbsolutePath();
        Path gradleWrapperPropertiesCloud = Paths.get(outputPathCloud + "/gradle/wrapper/gradle-wrapper.properties");
        Path gradleWrapperJarCloud = Paths.get(outputPathCloud + "/gradle/wrapper/gradle-wrapper.jar");
        Path gradleWrapperCloud = Paths.get(outputPathCloud + "/gradlew");
        Path gradleWrapperBatCloud = Paths.get(outputPathCloud + "/gradlew.bat");
        TestUtils.assertFileExists(gradleWrapperPropertiesCloud);
        TestUtils.assertFileExists(gradleWrapperCloud);
        TestUtils.assertFileExists(gradleWrapperBatCloud);
        //Different because file is not a text file
        assertTrue(Files.exists(gradleWrapperJarCloud));
    }
}
