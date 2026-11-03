/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openapitools.codegen.java.jaxrs;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.expr.AnnotationExpr;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.bind.Jsonb;
import jakarta.json.bind.JsonbBuilder;
import jakarta.json.stream.JsonParser;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AbstractJavaJAXRSServerCodegen;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import javax.tools.ToolProvider;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.StringReader;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.testng.Assert.*;

/**
 * Runtime regression test for null handling of the jaxrs-spec JSON-B models: a property that is both required and
 * nullable must be serialized as JSON {@code null}, while optional or non-nullable properties keep the JSON-B default
 * (omitted when null).
 */
public class JavaJAXRSSpecJsonbNullablePropertiesTest {

    private static final String MODEL_PACKAGE = "org.openapitools.model";

    @DataProvider
    public Object[][] specs() {
        return new Object[][]{
                {"src/test/resources/3_0/jaxrs-spec/jsonb-nullable-properties.yaml"},
                {"src/test/resources/3_1/jaxrs-spec/jsonb-nullable-properties.yaml"}
        };
    }

    @Test(dataProvider = "specs")
    public void testRequiredNullablePropertyIsSerializedAsNull(String spec) throws Exception {
        Path output = newTempFolder();
        File model = generateModel(spec, output, true);

        // Compile the generated model as is; only @Generated is dropped (jakarta.annotation is not on the test classpath).
        Path runtime = Files.createDirectory(output.resolve("runtime"));
        CompilationUnit unit = StaticJavaParser.parse(model);
        unit.findAll(AnnotationExpr.class).stream()
                .filter(annotation -> annotation.getNameAsString().endsWith("Generated"))
                .forEach(Node::remove);
        Path source = runtime.resolve("NullableProperties.java");
        Files.writeString(source, unit.toString());
        compile(runtime, source);

        try (URLClassLoader loader = new URLClassLoader(new URL[]{runtime.toUri().toURL()}, getClass().getClassLoader());
             Jsonb jsonb = JsonbBuilder.create()) {
            Class<?> type = loader.loadClass(MODEL_PACKAGE + ".NullableProperties");

            // all properties null: only the required nullable property is written (as JSON null)
            assertEquals(jsonb.toJson(type.getConstructor().newInstance()), "{\"requiredNullable\":null}");
            Object nulls = jsonb.fromJson("{\"requiredValue\":null,\"optionalValue\":null,\"requiredNullable\":null,"
                    + "\"optionalNullable\":null,\"snake_case_value\":null}", type);
            assertEquals(jsonb.toJson(nulls), "{\"requiredNullable\":null}");

            // non-null values, including the renamed property, are read and written unchanged
            String json = "{\"optionalNullable\":\"d\",\"optionalValue\":\"b\",\"requiredNullable\":\"c\","
                    + "\"requiredValue\":\"a\",\"snake_case_value\":\"e\"}";
            Object values = jsonb.fromJson(json, type);
            assertEquals(type.getMethod("getRequiredValue").invoke(values), "a");
            assertEquals(type.getMethod("getOptionalValue").invoke(values), "b");
            assertEquals(type.getMethod("getRequiredNullable").invoke(values), "c");
            assertEquals(type.getMethod("getOptionalNullable").invoke(values), "d");
            assertEquals(type.getMethod("getSnakeCaseValue").invoke(values), "e");
            assertEquals(parse(jsonb.toJson(values)), parse(json));
        }
    }

    @Test
    public void testJavaxRequiredNullablePropertyIsNillable() throws Exception {
        File model = generateModel("src/test/resources/3_0/jaxrs-spec/jsonb-nullable-properties.yaml", newTempFolder(), false);

        assertFileContains(model.toPath(),
                "@JsonbProperty(value = \"requiredNullable\", nillable = true)\n  public String getRequiredNullable()",
                "@JsonbProperty(\"requiredNullable\")\n  public void setRequiredNullable(",
                "@JsonbProperty(\"optionalNullable\")\n  public String getOptionalNullable()",
                "@JsonbProperty(\"requiredValue\")\n  public String getRequiredValue()");
        assertFileNotContains(model.toPath(),
                "@JsonbProperty(value = \"optionalNullable\", nillable = true)",
                "@JsonbProperty(value = \"requiredValue\", nillable = true)");
    }

    private static File generateModel(String spec, Path output, boolean useJakartaEe) {
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setValidateSpec(false)
                .setInputSpec(spec)
                .setOutputDir(output.toString())
                .addGlobalProperty("models", "")
                .addGlobalProperty("modelDocs", "false")
                .addGlobalProperty("modelTests", "false")
                .addAdditionalProperty(CodegenConstants.SERIALIZATION_LIBRARY, "jsonb")
                .addAdditionalProperty(AbstractJavaJAXRSServerCodegen.USE_JAKARTA_EE, useJakartaEe)
                .addAdditionalProperty("useBeanValidation", false)
                .addAdditionalProperty("useSwaggerAnnotations", false);
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        return files.stream().filter(file -> file.getName().equals("NullableProperties.java")).findFirst()
                .orElseThrow(() -> new AssertionError("NullableProperties.java was not generated"));
    }

    private static void compile(Path runtime, Path source) throws Exception {
        // the JSON-B and JSON-P API jars are enough to compile the generated model
        String classpath = jarOf(Jsonb.class) + File.pathSeparator + jarOf(JsonParser.class);
        ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
        assertNotNull(ToolProvider.getSystemJavaCompiler(), "Runtime regression tests require a JDK");
        int result = ToolProvider.getSystemJavaCompiler().run(null, diagnostics, diagnostics,
                "-classpath", classpath, "-d", runtime.toString(), source.toString());
        assertEquals(result, 0, diagnostics.toString(StandardCharsets.UTF_8));
    }

    private static String jarOf(Class<?> type) throws Exception {
        return Paths.get(type.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
    }

    private static JsonObject parse(String json) {
        return Json.createReader(new StringReader(json)).readObject();
    }
}
