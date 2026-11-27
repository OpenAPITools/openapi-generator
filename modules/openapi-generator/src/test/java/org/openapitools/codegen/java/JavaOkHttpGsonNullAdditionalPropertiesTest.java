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

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.gsonfire.GsonFireBuilder;
import okio.ByteString;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import javax.tools.ToolProvider;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
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
                .addGlobalProperty("models", "").addGlobalProperty("supportingFiles", "")
                .addGlobalProperty("modelDocs", "false").addGlobalProperty("modelTests", "false");
        new DefaultGenerator().opts(config.toClientOptInput()).generate();

        // Compile the generated client sources (the JSON factory and the model) against the real
        // Gson runtime, mirroring the sample-side JSONTest setup: the model's CustomTypeAdapterFactory
        // is registered by the generated JSON class, and JSON.java plus the generated model need the
        // client's runtime dependencies (gson-fire, okio, jsr305 @Nullable) on the compile classpath.
        Path generatedSource = output.resolve("src/main/java");
        Path jsonSource = generatedSource.resolve("org/openapitools/client/JSON.java");
        Path modelSource = generatedSource.resolve("org/openapitools/client/model/AdditionalPropertiesModel.java");
        String classpath = Paths.get(Gson.class.getProtectionDomain().getCodeSource().getLocation().toURI())
                + File.pathSeparator
                + Paths.get(GsonFireBuilder.class.getProtectionDomain().getCodeSource().getLocation().toURI())
                + File.pathSeparator
                + Paths.get(ByteString.class.getProtectionDomain().getCodeSource().getLocation().toURI())
                + File.pathSeparator
                + Paths.get(javax.annotation.Nullable.class.getProtectionDomain().getCodeSource().getLocation().toURI())
                + File.pathSeparator
                + Paths.get(kotlin.jvm.internal.Intrinsics.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
        assertNotNull(ToolProvider.getSystemJavaCompiler());
        assertEquals(ToolProvider.getSystemJavaCompiler().run(null, diagnostics, diagnostics,
                "--release", "8", "-classpath", classpath, "-d", output.toString(),
                jsonSource.toString(), modelSource.toString()),
                0, diagnostics.toString(StandardCharsets.UTF_8));

        try (URLClassLoader loader = new URLClassLoader(new URL[]{output.toUri().toURL()}, getClass().getClassLoader())) {
            Class<?> jsonType = loader.loadClass("org.openapitools.client.JSON");
            Class<?> modelType = loader.loadClass("org.openapitools.client.model.AdditionalPropertiesModel");
            // the client's configured Gson (JSON.getGson), not a hand-built one: its static
            // initializer registers the generated CustomTypeAdapterFactory for the model
            Gson gson = (Gson) jsonType.getMethod("getGson").invoke(null);
            // same pattern as the sample JSONTest: serializeNulls so named nulls are written
            Gson gsonWithNulls = gson.newBuilder().serializeNulls().create();

            // build the model with a null undeclared-property value
            Object model = modelType.getConstructor().newInstance();
            modelType.getMethod("setName", String.class).invoke(model, "x");
            modelType.getMethod("putAdditionalProperty", String.class, Object.class).invoke(model, "unknownField", null);

            // write: a null undeclared-property value used to throw
            // IllegalStateException("Not a JSON Object: null") in the CustomTypeAdapterFactory;
            // the ApiClient's writer omits named nulls, but must not throw
            assertEquals(gson.toJson(model, modelType), "{\"name\":\"x\"}");

            // serialize through the client's Gson and inspect the tree
            String serialized = gsonWithNulls.toJson(model, modelType);
            assertEquals(serialized, "{\"name\":\"x\",\"unknownField\":null}");
            JsonObject obj = gsonWithNulls.toJsonTree(model, modelType).getAsJsonObject();
            assertTrue(obj.get("unknownField").isJsonNull(), "A null additional property must be written as JSON null");
            assertEquals(obj.get("name").getAsString(), "x");
            assertNull(obj.get("additionalProperties"), "The raw additionalProperties field must be removed from the output");

            // full round trip: deserialize the resulting JSON (not a hand-written string)
            Object deserialized = gsonWithNulls.fromJson(serialized, modelType);
            assertEquals(modelType.getMethod("getName").invoke(deserialized), "x");
            Map<?, ?> props = (Map<?, ?>) modelType.getMethod("getAdditionalProperties").invoke(deserialized);
            assertTrue(props.containsKey("unknownField"), "A JSON null undeclared property must be preserved by the round trip");
            assertNull(props.get("unknownField"));

            // serialize again: the round trip must be byte-stable
            assertEquals(gsonWithNulls.toJson(deserialized, modelType), serialized, "Re-serialization must be byte-stable");
            assertEquals(gson.toJson(deserialized, modelType), "{\"name\":\"x\"}");

            // keep the existing behavior for non-null additional properties
            modelType.getMethod("putAdditionalProperty", String.class, Object.class).invoke(model, "extra", "e");
            modelType.getMethod("putAdditionalProperty", String.class, Object.class).invoke(model, "list", Arrays.asList("a", "b"));
            modelType.getMethod("putAdditionalProperty", String.class, Object.class).invoke(model, "nested", Collections.singletonMap("k", "v"));
            JsonElement tree = gsonWithNulls.toJsonTree(model, modelType);
            obj = tree.getAsJsonObject();
            assertEquals(obj.get("extra").getAsString(), "e");
            assertTrue(obj.get("list").isJsonArray());
            assertTrue(obj.get("nested").isJsonObject());
        }
    }
}
