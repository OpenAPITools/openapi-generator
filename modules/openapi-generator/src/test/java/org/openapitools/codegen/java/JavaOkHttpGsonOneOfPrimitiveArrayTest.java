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
import org.testng.annotations.Test;

import javax.tools.ToolProvider;
import java.io.ByteArrayOutputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.testng.Assert.*;

public class JavaOkHttpGsonOneOfPrimitiveArrayTest {
    @Test
    public void testPrimitiveArrayRoundTrip() throws Exception {
        Path output = newTempFolder();
        CodegenConfigurator config = new CodegenConfigurator().setGeneratorName("java").setLibrary("okhttp-gson")
                .setInputSpec("src/test/resources/3_0/java/oneof-primitive-array.yaml").setOutputDir(output.toString())
                .addGlobalProperty("models", "").addGlobalProperty("modelDocs", "false")
                .addGlobalProperty("modelTests", "false");
        new DefaultGenerator().opts(config.toClientOptInput()).generate();
        ClassOrInterfaceDeclaration union = StaticJavaParser.parse(output.resolve("src/main/java/org/openapitools/client/model/Union.java"))
                .getClassByName("Union").orElseThrow();
        ClassOrInterfaceDeclaration factory = union.getMembers().stream()
                .filter(m -> m instanceof ClassOrInterfaceDeclaration).map(m -> (ClassOrInterfaceDeclaration) m)
                .filter(c -> c.getNameAsString().equals("CustomTypeAdapterFactory")).findFirst().orElseThrow();

        // Exercise the generated factory with a minimal model stub and the real Gson runtime.
        String fixture = "import com.google.gson.*;\nimport com.google.gson.reflect.*;\n"
                + "import com.google.gson.stream.*;\nimport java.io.*;\nimport java.lang.reflect.Type;\n"
                + "import java.util.*;\nimport java.util.logging.*;\n"
                + "public class UnionFixture { public static class Union {\n"
                + "private static final Logger log = Logger.getLogger(Union.class.getName());\n"
                + "private Object value; public Object getActualInstance() { return value; }\n"
                + "public void setActualInstance(Object value) { this.value = value; }\n"
                + factory + "\n"
                + "public static class Variant { public String kind; "
                + "public static void validateJsonElement(JsonElement e) throws IOException { "
                + "if (!e.isJsonObject() || !e.getAsJsonObject().has(\"kind\")) throw new IOException(); } }\n"
                + "} }\n";
        Path source = output.resolve("UnionFixture.java");
        Files.writeString(source, fixture);
        String classpath = Paths.get(Gson.class.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
        assertNotNull(ToolProvider.getSystemJavaCompiler());
        assertEquals(ToolProvider.getSystemJavaCompiler().run(null, diagnostics, diagnostics,
                "--release", "8", "-classpath", classpath, "-d", output.toString(), source.toString()),
                0, diagnostics.toString(StandardCharsets.UTF_8));
        try (URLClassLoader loader = new URLClassLoader(new URL[]{output.toUri().toURL()}, getClass().getClassLoader())) {
            Class<?> type = loader.loadClass("UnionFixture$Union");
            TypeAdapterFactory instance = (TypeAdapterFactory) loader.loadClass("UnionFixture$Union$CustomTypeAdapterFactory")
                    .getConstructor().newInstance();
            Gson gson = new GsonBuilder().registerTypeAdapterFactory(instance).create();
            String json = "[\"alpha\",\"beta\"]";
            Object result = gson.fromJson(json, type);
            assertTrue(type.getMethod("getActualInstance").invoke(result) instanceof List);
            assertEquals(gson.toJson(result, type), json);
            json = "{\"kind\":\"known\"}";
            result = gson.fromJson(json, type);
            assertEquals(type.getMethod("getActualInstance").invoke(result).getClass().getSimpleName(), "Variant");
            assertEquals(gson.toJson(result, type), json);
        }
    }
}
