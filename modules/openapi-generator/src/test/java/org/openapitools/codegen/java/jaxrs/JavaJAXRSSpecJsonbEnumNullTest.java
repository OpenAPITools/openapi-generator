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
import com.github.javaparser.ast.body.EnumDeclaration;
import com.github.javaparser.ast.expr.AnnotationExpr;
import jakarta.json.bind.Jsonb;
import jakarta.json.bind.JsonbBuilder;
import jakarta.json.bind.JsonbException;
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
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.testng.Assert.*;

/**
 * Runtime regression test for the jaxrs-spec JSON-B enum (de)serializers: the generated enums are compiled and used
 * with a real JSON-B implementation, covering both enum templates (inline {@code enumClass} and {@code enumOuterClass}).
 */
public class JavaJAXRSSpecJsonbEnumNullTest {

    @Test
    public void testJsonbEnumDeserializersAcceptExplicitNull() throws Exception {
        Path output = newTempFolder();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setValidateSpec(false)
                .setInputSpec("src/test/resources/3_0/jaxrs-spec/petstore-with-fake-endpoints-models-for-testing.yaml")
                .setOutputDir(output.toString())
                .addGlobalProperty("models", "")
                .addGlobalProperty("modelDocs", "false")
                .addGlobalProperty("modelTests", "false")
                .addAdditionalProperty(CodegenConstants.SERIALIZATION_LIBRARY, "jsonb")
                .addAdditionalProperty(AbstractJavaJAXRSServerCodegen.USE_JAKARTA_EE, true);
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        // Compile the generated enums exactly as generated (including their JSON-B (de)serializers); only the
        // unrelated enclosing model classes are replaced by minimal holders with a single enum property.
        Path runtime = Files.createDirectory(output.resolve("runtime"));
        String imports = "import jakarta.json.bind.annotation.*;\n"
                + "import jakarta.json.bind.serializer.*;\n"
                + "import jakarta.json.stream.*;\n"
                + "import java.lang.reflect.Type;\n";
        Files.writeString(runtime.resolve("PetHolder.java"), imports
                + "public class PetHolder {\n"
                + "    public StatusEnum status;\n"
                + enumDeclaration(files, "Pet.java", "StatusEnum") + "\n}\n");
        Files.writeString(runtime.resolve("OuterEnum.java"), imports + enumDeclaration(files, "OuterEnum.java", "OuterEnum"));
        Files.writeString(runtime.resolve("OuterEnumInteger.java"), imports + enumDeclaration(files, "OuterEnumInteger.java", "OuterEnumInteger"));
        Files.writeString(runtime.resolve("OuterHolder.java"), "public class OuterHolder {\n"
                + "    public OuterEnum outerEnum;\n"
                + "    public OuterEnumInteger outerEnumInteger;\n"
                + "}\n");
        compile(runtime);

        try (URLClassLoader loader = new URLClassLoader(new URL[]{runtime.toUri().toURL()}, getClass().getClassLoader());
             Jsonb jsonb = JsonbBuilder.create()) {
            Class<?> petHolder = loader.loadClass("PetHolder");
            Class<?> outerHolder = loader.loadClass("OuterHolder");

            // current non-null behavior: the enum wire values
            Object pet = jsonb.fromJson("{\"status\":\"available\"}", petHolder);
            assertEquals(String.valueOf(petHolder.getField("status").get(pet)), "available");
            assertEquals(jsonb.toJson(pet), "{\"status\":\"available\"}");
            Object outer = jsonb.fromJson("{\"outerEnum\":\"approved\",\"outerEnumInteger\":1}", outerHolder);
            assertEquals(String.valueOf(outerHolder.getField("outerEnum").get(outer)), "approved");
            assertEquals(String.valueOf(outerHolder.getField("outerEnumInteger").get(outer)), "1");

            // explicit JSON null must deserialize to null for both enum templates
            assertNull(petHolder.getField("status").get(jsonb.fromJson("{\"status\":null}", petHolder)));
            Object nullOuter = jsonb.fromJson("{\"outerEnum\":null,\"outerEnumInteger\":null}", outerHolder);
            assertNull(outerHolder.getField("outerEnum").get(nullOuter));
            assertNull(outerHolder.getField("outerEnumInteger").get(nullOuter));
        }
    }

    @Test
    public void testJsonbEnumsCompileNextToSchemaNamedType() throws Exception {
        // the generated top-level enum 'Type' and the model with an inline enum that references it
        Path runtime = compileGeneratedModels("src/test/resources/3_0/jaxrs-spec/jsonb-enum-named-type.yaml", Map.of(),
                "Type.java", "TypeHolder.java");

        try (URLClassLoader loader = new URLClassLoader(new URL[]{runtime.toUri().toURL()}, getClass().getClassLoader());
             Jsonb jsonb = JsonbBuilder.create()) {
            Class<?> holder = loader.loadClass("org.openapitools.model.TypeHolder");
            String json = "{\"kind\":\"beta\",\"status\":\"pending\"}";
            assertEquals(jsonb.toJson(jsonb.fromJson(json, holder)), json);
        }
    }

    @DataProvider
    public Object[][] enumOptions() {
        return new Object[][]{{false, false}, {true, false}, {false, true}, {true, true}};
    }

    /**
     * The JSON-B deserializers must resolve values like the generated {@code fromValue} (used by Jackson), so that
     * {@code useEnumCaseInsensitive} and {@code enumUnknownDefaultCase} apply to top-level, inline and list item enums.
     */
    @Test(dataProvider = "enumOptions")
    public void testJsonbEnumDeserializersUseFromValueOptions(boolean caseInsensitive, boolean unknownDefault) throws Exception {
        Path runtime = compileGeneratedModels("src/test/resources/3_0/jaxrs-spec/jsonb-enum-options.yaml",
                Map.of("useEnumCaseInsensitive", caseInsensitive, "enumUnknownDefaultCase", unknownDefault),
                "StringEnum.java", "IntegerEnum.java", "NumberEnum.java", "EnumHolder.java");
        String otherCase = caseInsensitive ? "AVAILABLE" : unknownDefault ? "UNKNOWN_DEFAULT_OPEN_API" : "rejected";
        String unknown = unknownDefault ? "UNKNOWN_DEFAULT_OPEN_API" : "rejected";
        String unknownNumber = unknownDefault ? "NUMBER_unknown_default_open_api" : "rejected";

        try (URLClassLoader loader = new URLClassLoader(new URL[]{runtime.toUri().toURL()}, getClass().getClassLoader());
             Jsonb jsonb = JsonbBuilder.create()) {
            Class<?> holder = loader.loadClass("org.openapitools.model.EnumHolder");
            for (String property : List.of("top", "inline")) {
                assertEquals(read(jsonb, holder, property, "\"available\""), "AVAILABLE", property);
                assertEquals(read(jsonb, holder, property, "\"AVAILABLE\""), otherCase, property);
                assertEquals(read(jsonb, holder, property, "\"unknown\""), unknown, property);
                assertEquals(read(jsonb, holder, property, "null"), "null", property);
            }
            for (String property : List.of("topInteger", "inlineInteger")) {
                assertEquals(read(jsonb, holder, property, "2"), "NUMBER_2", property);
                assertEquals(read(jsonb, holder, property, "9"), unknownNumber, property);
                assertEquals(read(jsonb, holder, property, "null"), "null", property);
            }
            assertEquals(read(jsonb, holder, "topNumber", "-2.25"), "NUMBER_MINUS_2_DOT_25");
            assertEquals(read(jsonb, holder, "topNumber", "9"), unknownNumber);
            assertEquals(read(jsonb, holder, "topNumber", "null"), "null");
            assertEquals(read(jsonb, holder, "list", "[\"pending\",null]"), "[PENDING, null]");
            assertEquals(read(jsonb, holder, "list", "[\"AVAILABLE\"]"), otherCase.equals("rejected") ? "rejected" : "[" + otherCase + "]");
            assertEquals(read(jsonb, holder, "list", "[\"unknown\"]"), unknown.equals("rejected") ? "rejected" : "[" + unknown + "]");

            // values are still written as their wire values
            Object values = jsonb.fromJson("{\"inline\":\"pending\",\"inlineInteger\":1,\"list\":[\"available\"],\"top\":\"pending\","
                    + "\"topInteger\":2,\"topNumber\":1.5}", holder);
            assertEquals(jsonb.toJson(values), "{\"inline\":\"pending\",\"inlineInteger\":1,\"list\":[\"available\"],\"top\":\"pending\","
                    + "\"topInteger\":2,\"topNumber\":1.5}");
        }
    }

    /** The enum constant(s) read into the given property, or "rejected" if JSON-B fails to deserialize the value. */
    private static String read(Jsonb jsonb, Class<?> holder, String property, String jsonValue) throws Exception {
        Object instance;
        try {
            instance = jsonb.fromJson("{\"" + property + "\":" + jsonValue + "}", holder);
        } catch (JsonbException e) {
            return "rejected";
        }
        Object value = holder.getMethod("get" + Character.toUpperCase(property.charAt(0)) + property.substring(1)).invoke(instance);
        if (value instanceof List) {
            return ((List<?>) value).stream().map(e -> e == null ? "null" : ((Enum<?>) e).name()).collect(Collectors.toList()).toString();
        }
        return value == null ? "null" : ((Enum<?>) value).name();
    }

    /**
     * Generates the jaxrs-spec JSON-B models of the given spec and compiles the given model files as generated; only
     * {@code @Generated} is dropped (jakarta.annotation is not on the test classpath).
     */
    private static Path compileGeneratedModels(String spec, Map<String, Object> properties, String... fileNames) throws Exception {
        Path output = newTempFolder();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setValidateSpec(false)
                .setInputSpec(spec)
                .setOutputDir(output.toString())
                .addGlobalProperty("models", "")
                .addGlobalProperty("modelDocs", "false")
                .addGlobalProperty("modelTests", "false")
                .addAdditionalProperty(CodegenConstants.SERIALIZATION_LIBRARY, "jsonb")
                .addAdditionalProperty(AbstractJavaJAXRSServerCodegen.USE_JAKARTA_EE, true)
                .addAdditionalProperty("useBeanValidation", false)
                .addAdditionalProperty("useSwaggerAnnotations", false);
        properties.forEach(configurator::addAdditionalProperty);
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        Path runtime = Files.createDirectory(output.resolve("runtime"));
        for (String fileName : fileNames) {
            File file = files.stream().filter(f -> f.getName().equals(fileName)).findFirst()
                    .orElseThrow(() -> new AssertionError(fileName + " was not generated"));
            CompilationUnit unit = StaticJavaParser.parse(file);
            unit.findAll(AnnotationExpr.class).stream()
                    .filter(annotation -> annotation.getNameAsString().endsWith("Generated"))
                    .forEach(Node::remove);
            Files.writeString(runtime.resolve(fileName), unit.toString());
        }
        compile(runtime);
        return runtime;
    }

    /** The generated enum declaration with its JSON-B annotations; other annotations (e.g. @Generated) are dropped. */
    private static String enumDeclaration(List<File> files, String fileName, String enumName) throws Exception {
        File file = files.stream().filter(f -> f.getName().equals(fileName)).findFirst()
                .orElseThrow(() -> new AssertionError(fileName + " was not generated"));
        CompilationUnit unit = StaticJavaParser.parse(file);
        EnumDeclaration declaration = unit.findAll(EnumDeclaration.class).stream()
                .filter(d -> d.getNameAsString().equals(enumName))
                .findFirst().orElseThrow(() -> new AssertionError(enumName + " was not generated in " + fileName));
        declaration.getAnnotations().removeIf(a -> !a.getNameAsString().startsWith("Jsonb"));
        return declaration.toString();
    }

    private static void compile(Path runtime) throws Exception {
        // the JSON-B and JSON-P API jars are enough to compile the generated (de)serializers
        String classpath = jarOf(Jsonb.class) + File.pathSeparator + jarOf(JsonParser.class);
        List<String> args = new ArrayList<>(List.of("-classpath", classpath, "-d", runtime.toString()));
        try (Stream<Path> sources = Files.list(runtime)) {
            sources.filter(p -> p.toString().endsWith(".java")).map(Path::toString).forEach(args::add);
        }
        ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
        assertNotNull(ToolProvider.getSystemJavaCompiler(), "Runtime regression tests require a JDK");
        int result = ToolProvider.getSystemJavaCompiler().run(null, diagnostics, diagnostics, args.toArray(new String[0]));
        assertEquals(result, 0, diagnostics.toString(StandardCharsets.UTF_8));
    }

    private static String jarOf(Class<?> type) throws Exception {
        return Paths.get(type.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
    }
}
