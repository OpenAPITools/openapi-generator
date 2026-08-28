/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.java.jersey3;

import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.testng.Assert.assertThrows;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;

/**
 * Verifies that the jersey3 library generates Jackson-3-compatible code
 * when {@code useJackson3=true} is set.
 */
public class JavaJersey3Jackson3Test {

    private static final String PETSTORE_SPEC = "src/test/resources/3_0/petstore.yaml";

    /**
     * useJackson3=true combined with library=jersey3 should succeed (regression guard).
     * Before this feature, JavaClientCodegen.processOpts threw IllegalArgumentException
     * for any library other than native/apache-httpclient/spring-*.
     */
    @Test
    public void jersey3WithJackson3DoesNotThrow() throws Exception {
        Path output = Files.createTempDirectory("jersey3-jackson3-positive");
        output.toFile().deleteOnExit();

        List<File> files = generate(output, true);
        assertTrue(!files.isEmpty(), "Generation should produce files");
    }

    /**
     * useJackson3=true with an unsupported library should still throw. Sanity check that
     * we didn't accidentally open the gate too wide.
     */
    @Test
    public void jackson3WithUnsupportedLibraryStillThrows() throws Exception {
        Path output = Files.createTempDirectory("jersey3-jackson3-negative");
        output.toFile().deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec(PETSTORE_SPEC);
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setLibrary(JavaClientCodegen.JERSEY2);
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.toAbsolutePath().toString());
        codegen.additionalProperties().put(JavaClientCodegen.USE_JACKSON_3, "true");

        ClientOptInput input = new ClientOptInput().openAPI(openAPI).config(codegen);
        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);

        assertThrows(IllegalArgumentException.class, () -> generator.opts(input).generate());
    }

    /**
     * Generated pom.xml should pull tools.jackson.* artifacts (not com.fasterxml.jackson.core)
     * and add the Jackson 3 JAX-RS provider (jackson-jakarta-rs-json-provider) instead of
     * jersey-media-json-jackson (which is Jackson 2 only).
     */
    @Test
    public void pomReferencesJackson3Artifacts() throws Exception {
        Path output = Files.createTempDirectory("jersey3-jackson3-pom");
        output.toFile().deleteOnExit();
        generate(output, true);

        String pom = Files.readString(output.resolve("pom.xml"));
        assertTrue(pom.contains("<groupId>tools.jackson.core</groupId>"),
                "pom.xml should depend on tools.jackson.core artifacts");
        assertTrue(pom.contains("jackson-jakarta-rs-json-provider"),
                "pom.xml should pull the Jackson 3 JAX-RS provider");
        assertTrue(!pom.contains("<artifactId>jersey-media-json-jackson</artifactId>"),
                "pom.xml should not pull Jackson-2-only jersey-media-json-jackson");
        assertTrue(pom.contains("<jackson3-version>"),
                "pom.xml should declare a jackson3-version property");
    }

    /**
     * Generated JSON.java should import {@code tools.jackson.databind.*} and register the
     * {@link tools.jackson.databind.json.JsonMapper}-style builder, plus
     * {@code JsonNullableJackson3Module} (not the Jackson-2 one).
     */
    @Test
    public void jsonClassUsesJackson3Apis() throws Exception {
        Path output = Files.createTempDirectory("jersey3-jackson3-json");
        output.toFile().deleteOnExit();
        generate(output, true);

        Path jsonJava = findFirst(output, "JSON.java");
        String contents = Files.readString(jsonJava);
        assertTrue(contents.contains("import tools.jackson.databind."),
                "JSON.java should import tools.jackson.databind.*");
        assertTrue(contents.contains("import tools.jackson.databind.json.JsonMapper;"),
                "JSON.java should import JsonMapper from tools.jackson");
        assertTrue(!contents.contains("import com.fasterxml.jackson.databind.json.JsonMapper;"),
                "JSON.java should not import the Jackson 2 JsonMapper");
        assertTrue(!contents.contains("import org.openapitools.jackson.nullable.JsonNullableModule;"),
                "JSON.java should not import the Jackson 2 JsonNullableModule");
    }

    /**
     * Generated ApiClient.java should register the Jackson 3 JAX-RS provider in place of
     * Jersey's JacksonFeature (which is hard-wired to Jackson 2).
     */
    @Test
    public void apiClientRegistersJackson3Provider() throws Exception {
        Path output = Files.createTempDirectory("jersey3-jackson3-apiclient");
        output.toFile().deleteOnExit();
        generate(output, true);

        Path apiClient = findFirst(output, "ApiClient.java");
        String contents = Files.readString(apiClient);
        assertTrue(contents.contains("tools.jackson.jakarta.rs.json.JacksonJsonProvider"),
                "ApiClient.java should import the Jackson 3 JAX-RS JsonProvider");
        assertTrue(!contents.contains("org.glassfish.jersey.jackson.JacksonFeature"),
                "ApiClient.java should not import Jersey's Jackson 2-only JacksonFeature");
    }

    /**
     * With useJackson3=false (the default), generated code should remain on Jackson 2 —
     * guards against silently flipping the default.
     */
    @Test
    public void defaultIsJackson2() throws Exception {
        Path output = Files.createTempDirectory("jersey3-jackson2-default");
        output.toFile().deleteOnExit();
        generate(output, false);

        Path apiClient = findFirst(output, "ApiClient.java");
        String contents = Files.readString(apiClient);
        assertTrue(contents.contains("org.glassfish.jersey.jackson.JacksonFeature"),
                "Default (useJackson3=false) should still use Jersey's Jackson 2 JacksonFeature");
        assertTrue(!contents.contains("tools.jackson"),
                "Default ApiClient.java should not reference tools.jackson");
    }

    private List<File> generate(Path outputDir, boolean useJackson3) throws Exception {
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.JERSEY3)
                .setInputSpec(PETSTORE_SPEC)
                .setOutputDir(outputDir.toAbsolutePath().toString());
        configurator.addAdditionalProperty(JavaClientCodegen.USE_JACKSON_3, useJackson3);
        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        return generator.opts(configurator.toClientOptInput()).generate();
    }

    private Path findFirst(Path root, String fileName) throws Exception {
        try (var stream = Files.walk(root)) {
            return stream.filter(p -> p.getFileName().toString().equals(fileName))
                    .findFirst()
                    .orElseThrow(() -> new AssertionError(fileName + " not generated under " + root));
        }
    }
}
