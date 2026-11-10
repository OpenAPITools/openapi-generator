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
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.TypeAdapterFactory;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import javax.tools.ToolProvider;
import java.io.ByteArrayOutputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.testng.Assert.*;

public class JavaOkHttpGsonNullAdditionalPropertiesTest {

    @Test
    public void testNullAdditionalPropertiesRoundTrip() throws Exception {
        Path output = newTempFolder();
        CodegenConfigurator config = new CodegenConfigurator().setGeneratorName("java").setLibrary("okhttp-gson")
                .setInputSpec("src/test/resources/3_0/java/okhttp-gson-null-additional-properties.yaml")
                .setOutputDir(output.toString())
                .addGlobalProperty("models", "").addGlobalProperty("modelDocs", "false")
                .addGlobalProperty("modelTests", "false");
        new DefaultGenerator().opts(config.toClientOptInput()).generate();
        ClassOrInterfaceDeclaration modelClass = StaticJavaParser.parse(
                output.resolve("src/main/java/org/openapitools/client/model/AdditionalPropertiesModel.java"))
                .getClassByName("AdditionalPropertiesModel").orElseThrow();
        ClassOrInterfaceDeclaration factory = modelClass.getMembers().stream()
                .filter(m -> m instanceof ClassOrInterfaceDeclaration).map(m -> (ClassOrInterfaceDeclaration) m)
                .filter(c -> c.getNameAsString().equals("CustomTypeAdapterFactory")).findFirst().orElseThrow();

        // Exercise the generated factory with a minimal model stub and the real Gson runtime.
        String fixture = "import com.google.gson.*;\nimport com.google.gson.reflect.*;\n"
                + "import com.google.gson.stream.*;\nimport java.io.*;\nimport java.lang.reflect.Type;\n"
                + "import java.util.*;\n"
                + "public class ModelFixture { public static class AdditionalPropertiesModel {\n"
                + "private String name;\n"
                + "private Map<String, Object> additionalProperties;\n"
                + "public String getName() { return name; }\n"
                + "public void setName(String name) { this.name = name; }\n"
                + "public Map<String, Object> getAdditionalProperties() { return additionalProperties; }\n"
                + "public void putAdditionalProperty(String key, Object value) { "
                + "if (this.additionalProperties == null) { this.additionalProperties = new HashMap<String, Object>(); } "
                + "this.additionalProperties.put(key, value); }\n"
                + "public static HashSet<String> openapiFields = new HashSet<String>(Arrays.asList(\"name\"));\n"
                + "public static void validateJsonElement(JsonElement jsonElement) throws IOException { }\n"
                + factory + "\n"
                + "} }\n";
        Path source = output.resolve("ModelFixture.java");
        Files.writeString(source, fixture);
        String classpath = Paths.get(Gson.class.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
        assertNotNull(ToolProvider.getSystemJavaCompiler());
        assertEquals(ToolProvider.getSystemJavaCompiler().run(null, diagnostics, diagnostics,
                "--release", "8", "-classpath", classpath, "-d", output.toString(), source.toString()),
                0, diagnostics.toString(StandardCharsets.UTF_8));

        try (URLClassLoader loader = new URLClassLoader(new URL[]{output.toUri().toURL()}, getClass().getClassLoader())) {
            Class<?> type = loader.loadClass("ModelFixture$AdditionalPropertiesModel");
            TypeAdapterFactory factoryInstance = (TypeAdapterFactory) loader
                    .loadClass("ModelFixture$AdditionalPropertiesModel$CustomTypeAdapterFactory")
                    .getConstructor().newInstance();
            // mirror the ApiClient's Gson, which does not enable serializeNulls
            Gson gson = new GsonBuilder().registerTypeAdapterFactory(factoryInstance).create();
            Gson gsonWithNulls = new GsonBuilder().registerTypeAdapterFactory(factoryInstance).serializeNulls().create();
            java.lang.reflect.Method putAdditionalProperty = type.getMethod("putAdditionalProperty", String.class, Object.class);

            // write: a null undeclared-property value used to throw
            // IllegalStateException("Not a JSON Object: null") in the CustomTypeAdapterFactory
            Object model = type.getConstructor().newInstance();
            type.getMethod("setName", String.class).invoke(model, "x");
            putAdditionalProperty.invoke(model, "unknownField", null);
            // JsonElement-level check (serializeNulls enabled, since Gson drops named nulls otherwise)
            JsonObject obj = gsonWithNulls.toJsonTree(model, type).getAsJsonObject();
            assertTrue(obj.get("unknownField").isJsonNull(), "A null additional property must be written as JSON null");
            assertEquals(obj.get("name").getAsString(), "x");
            assertNull(obj.get("additionalProperties"), "The raw additionalProperties field must be removed from the output");
            // the ApiClient's writer omits named nulls, but must not throw
            assertEquals(gson.toJson(model, type), "{\"name\":\"x\"}");
            assertEquals(gsonWithNulls.toJson(model, type), "{\"name\":\"x\",\"unknownField\":null}");

            // keep the existing behavior for non-null additional properties
            putAdditionalProperty.invoke(model, "extra", "e");
            putAdditionalProperty.invoke(model, "list", Arrays.asList("a", "b"));
            putAdditionalProperty.invoke(model, "nested", Collections.singletonMap("k", "v"));
            JsonElement tree = gson.toJsonTree(model, type);
            obj = tree.getAsJsonObject();
            assertEquals(obj.get("extra").getAsString(), "e");
            assertTrue(obj.get("list").isJsonArray());
            assertTrue(obj.get("nested").isJsonObject());

            // read: JSON null undeclared properties deserialize into null-valued entries
            Object deserialized = gson.fromJson("{\"name\":\"x\",\"unknownField\":null}", type);
            assertEquals(type.getMethod("getName").invoke(deserialized), "x");
            Map<?, ?> props = (Map<?, ?>) type.getMethod("getAdditionalProperties").invoke(deserialized);
            assertTrue(props.containsKey("unknownField"), "A JSON null undeclared property must be stored");
            assertNull(props.get("unknownField"));

            // serialize -> deserialize -> serialize round trip preserves the null entry
            assertEquals(gsonWithNulls.toJson(deserialized, type), "{\"name\":\"x\",\"unknownField\":null}");
            assertEquals(gson.toJson(deserialized, type), "{\"name\":\"x\"}");
        }
    }
}
