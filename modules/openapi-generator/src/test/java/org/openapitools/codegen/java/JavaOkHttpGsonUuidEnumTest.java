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
package org.openapitools.codegen.java;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.body.EnumDeclaration;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import javax.tools.ToolProvider;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.testng.Assert.*;

public class JavaOkHttpGsonUuidEnumTest {

    @DataProvider
    public Object[][] caseInsensitiveOptions() {
        return new Object[][]{{false}, {true}};
    }

    @Test(dataProvider = "caseInsensitiveOptions")
    public void testUuidEnumGsonRoundTrip(boolean caseInsensitive) throws Exception {
        Path output = newTempFolder();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary("okhttp-gson")
                .setInputSpec("src/test/resources/3_0/issue_20952_inner_enum_comparison.yaml")
                .setOutputDir(output.toString())
                .addGlobalProperty("models", "")
                .addGlobalProperty("modelDocs", "false")
                .addGlobalProperty("modelTests", "false")
                .addAdditionalProperty("useEnumCaseInsensitive", caseInsensitive);
        File model = new DefaultGenerator().opts(configurator.toClientOptInput()).generate().stream()
                .filter(file -> file.getName().equals("Example.java"))
                .findFirst().orElseThrow(() -> new AssertionError("Example.java was not generated"));
        EnumDeclaration uuidEnum = StaticJavaParser.parse(model).findAll(EnumDeclaration.class).stream()
                .filter(declaration -> declaration.getNameAsString().equals("UuidEnum"))
                .findFirst().orElseThrow(() -> new AssertionError("UuidEnum was not generated"));

        // Compile the complete generated enum, including its Gson adapter and validation method.
        // Only the enclosing model and its unrelated client dependencies are omitted.
        Path runtime = Files.createDirectory(output.resolve("runtime"));
        Path fixture = runtime.resolve("EnumFixture.java");
        Files.writeString(fixture, "import com.google.gson.*;\n"
                + "import com.google.gson.annotations.JsonAdapter;\n"
                + "import com.google.gson.stream.*;\n"
                + "import java.io.IOException;\n"
                + "import java.util.UUID;\n"
                + "public class EnumFixture {\n" + uuidEnum + "\n}\n");
        String gsonClasspath = Paths.get(Gson.class.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
        assertNotNull(ToolProvider.getSystemJavaCompiler(), "Runtime regression tests require a JDK");
        int result = ToolProvider.getSystemJavaCompiler().run(null, diagnostics, diagnostics,
                "-classpath", gsonClasspath, "-d", runtime.toString(), fixture.toString());
        assertEquals(result, 0, diagnostics.toString(StandardCharsets.UTF_8));

        try (URLClassLoader loader = new URLClassLoader(new URL[]{runtime.toUri().toURL()}, getClass().getClassLoader())) {
            Class<?> enumClass = loader.loadClass("EnumFixture$UuidEnum");
            Method validate = enumClass.getMethod("validateJsonElement", JsonElement.class);
            Gson gson = new Gson();
            String[] values = {
                    "123e4567-e89b-12d3-a456-426614174000",
                    "123e4567-e89b-12d3-a456-426614174001"
            };
            Object[] constants = enumClass.getEnumConstants();
            assertEquals(constants.length, values.length);
            for (int i = 0; i < values.length; i++) {
                String json = "\"" + values[i] + "\"";
                assertEquals(gson.toJson(constants[i]), json);
                assertSame(gson.fromJson(json, enumClass), constants[i]);
                validate.invoke(null, new JsonPrimitive(values[i]));
            }
            for (String invalid : new String[]{"not-a-uuid", "123e4567-e89b-12d3-a456-426614174002"}) {
                expectThrows(IllegalArgumentException.class,
                        () -> gson.fromJson(new JsonPrimitive(invalid), enumClass));
                InvocationTargetException exception = expectThrows(InvocationTargetException.class,
                        () -> validate.invoke(null, new JsonPrimitive(invalid)));
                assertTrue(exception.getCause() instanceof IllegalArgumentException,
                        "Validation should reject malformed or unknown UUIDs: " + exception.getCause());
            }
        }
    }
}
