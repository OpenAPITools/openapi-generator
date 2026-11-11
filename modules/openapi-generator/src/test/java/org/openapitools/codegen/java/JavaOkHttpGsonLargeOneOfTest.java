/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 */
package org.openapitools.codegen.java;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.TypeAdapterFactory;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import javax.tools.ToolProvider;
import java.io.ByteArrayOutputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.testng.Assert.*;

public class JavaOkHttpGsonLargeOneOfTest {
    @DataProvider
    public Object[][] variantCounts() {
        return new Object[][]{{2}, {100}, {101}, {260}};
    }

    @Test(dataProvider = "variantCounts")
    public void testOneOfAdapterLoadsAndRoundTrips(int count) throws Exception {
        Path output = newTempFolder();
        Path spec = output.resolve("oneof.yaml");
        StringBuilder yaml = new StringBuilder("openapi: 3.0.3\ninfo: {title: OneOf, version: '1'}\npaths: {}\ncomponents:\n  schemas:\n    Union:\n      oneOf:\n");
        for (int i = 0; i < count; i++) yaml.append("        - $ref: '#/components/schemas/Variant").append(i).append("'\n");
        if (count == 260) {
            yaml.append("        - type: array\n          items: {$ref: '#/components/schemas/Variant0'}\n")
                    .append("        - type: object\n          additionalProperties: {type: integer}\n");
        }
        for (int i = 0; i < count; i++) yaml.append("    Variant").append(i).append(":\n      type: object\n      properties:\n        kind: {type: string}\n");
        Files.writeString(spec, yaml.toString());
        CodegenConfigurator config = new CodegenConfigurator().setGeneratorName("java").setLibrary("okhttp-gson")
                .setInputSpec(spec.toString()).setOutputDir(output.toString())
                .addGlobalProperty("models", "").addGlobalProperty("modelDocs", "false")
                .addGlobalProperty("modelTests", "false");
        new DefaultGenerator().opts(config.toClientOptInput()).generate();
        ClassOrInterfaceDeclaration union = StaticJavaParser.parse(output.resolve("src/main/java/org/openapitools/client/model/Union.java"))
                .getClassByName("Union").orElseThrow();
        ClassOrInterfaceDeclaration factory = union.getMembers().stream()
                .filter(m -> m instanceof ClassOrInterfaceDeclaration)
                .map(m -> (ClassOrInterfaceDeclaration) m).filter(c -> c.getNameAsString().equals("CustomTypeAdapterFactory"))
                .findFirst().orElseThrow();

        // Compile the generated factory with minimal model stubs, so the generated anonymous adapter
        // is loaded and exercised without requiring a separate generated project or Maven invocation.
        StringBuilder fixture = new StringBuilder("import com.google.gson.*;\nimport com.google.gson.reflect.*;\n"
                + "import com.google.gson.stream.*;\nimport java.io.*;\nimport java.lang.reflect.Type;\nimport java.util.*;\nimport java.util.logging.*;\n"
                + "public class UnionFixture { public static class Union {\n"
                + "private static final Logger log = Logger.getLogger(Union.class.getName());\n"
                + "private Object value; public Object getActualInstance() { return value; }\n"
                + "public void setActualInstance(Object value) { this.value = value; }\n");
        fixture.append(factory).append('\n');
        for (int i = 0; i < count; i++) {
            fixture.append("public static class Variant").append(i).append(" { public String kind; ")
                    .append("public static void validateJsonElement(JsonElement e) throws IOException { ")
                    .append("if (!e.getAsJsonObject().get(\"kind\").getAsString().equals(\"v")
                    .append(i).append("\")) throw new IOException(); } }\n");
        }
        fixture.append("} }\n");
        Path source = output.resolve("UnionFixture.java");
        Files.writeString(source, fixture.toString());
        String gsonClasspath = Paths.get(Gson.class.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
        assertNotNull(ToolProvider.getSystemJavaCompiler());
        assertEquals(ToolProvider.getSystemJavaCompiler().run(null, diagnostics, diagnostics,
                "--release", "8", "-classpath", gsonClasspath, "-d", output.toString(), source.toString()),
                0, diagnostics.toString(StandardCharsets.UTF_8));
        try (URLClassLoader loader = new URLClassLoader(new URL[]{output.toUri().toURL()}, getClass().getClassLoader())) {
            Class<?> type = loader.loadClass("UnionFixture$Union");
            TypeAdapterFactory instance = (TypeAdapterFactory) loader.loadClass("UnionFixture$Union$CustomTypeAdapterFactory")
                    .getConstructor().newInstance();
            Gson gson = new GsonBuilder().registerTypeAdapterFactory(instance).create();
            for (int i : new int[]{0, count - 1}) {
                String json = "{\"kind\":\"v" + i + "\"}";
                Object result = gson.fromJson(json, type);
                Object value = type.getMethod("getActualInstance").invoke(result);
                assertEquals(value.getClass().getSimpleName(), "Variant" + i);
                assertEquals(gson.toJson(result, type), json);
            }
            if (count == 260) {
                for (String json : new String[]{"[{\"kind\":\"v0\"}]", "{\"count\":3}"}) {
                    Object result = gson.fromJson(json, type);
                    Object value = type.getMethod("getActualInstance").invoke(result);
                    assertTrue(json.startsWith("[") ? value instanceof java.util.List : value instanceof java.util.Map);
                    assertEquals(gson.toJson(result, type), json);
                }
            }
        }
    }
}
