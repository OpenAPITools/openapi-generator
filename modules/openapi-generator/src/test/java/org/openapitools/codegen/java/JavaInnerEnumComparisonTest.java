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

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import javax.tools.ToolProvider;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.testng.Assert.*;

public class JavaInnerEnumComparisonTest {

    @DataProvider
    public Object[][] generators() {
        // Cover every affected inner-enum partial and both users of shared CXF/Micronaut partials.
        String[][] configurations = {
                {"java", "okhttp-gson"},
                {"java", "resttemplate"},
                {"java", "microprofile"},
                {"spring", null},
                {"jaxrs-jersey", null},
                {"jaxrs-spec", null},
                {"jaxrs-cxf", null},
                {"jaxrs-cxf-client", null},
                {"jaxrs-cxf-cdi", null},
                {"jaxrs-cxf-extended", null},
                {"java-play-framework", null},
                {"java-micronaut-client", null},
                {"java-micronaut-server", null}
        };
        List<Object[]> cases = new ArrayList<>();
        for (String[] configuration : configurations) {
            for (boolean caseInsensitive : new boolean[]{false, true}) {
                cases.add(new Object[]{configuration[0], configuration[1], caseInsensitive});
            }
        }
        return cases.toArray(new Object[0][]);
    }

    @Test(dataProvider = "generators")
    public void testInnerEnumComparisons(String generator, String library, boolean caseInsensitive) throws Exception {
        Path output = newTempFolder();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName(generator)
                .setInputSpec("src/test/resources/3_0/issue_20952_inner_enum_comparison.yaml")
                .setOutputDir(output.toString())
                .addGlobalProperty("models", "")
                .addGlobalProperty("modelDocs", "false")
                .addGlobalProperty("modelTests", "false")
                .addAdditionalProperty("useEnumCaseInsensitive", caseInsensitive);
        if (library != null) {
            configurator.setLibrary(library);
        }
        if ("microprofile".equals(library)) {
            configurator.addAdditionalProperty("serializationLibrary", "jackson");
        }
        File model = new DefaultGenerator().opts(configurator.toClientOptInput()).generate().stream()
                .filter(file -> file.getName().equals("Example.java"))
                .findFirst().orElseThrow(() -> new AssertionError("Example.java was not generated"));

        String source = Files.readString(model.toPath());
        assertComparison(source, "PlainStringEnum",
                caseInsensitive ? "b.value.equalsIgnoreCase(value)" : "b.value.equals(value)");
        String formattedComparison = caseInsensitive
                ? "b.value.toString().equalsIgnoreCase(value == null ? null : value.toString())"
                : "b.value.equals(value)";
        for (String enumName : new String[]{"FormattedStringEnum", "EmailEnum", "NullableStringEnum",
                "ExampleInnerThreeEnum", "NullableUriEnum", "UuidEnum"}) {
            assertComparison(source, enumName, formattedComparison);
        }
        assertComparison(source, "NumberEnum", "b.value.equals(value)");
        assertComparison(source, "FormattedNumberEnum", "b.value.equals(value)");
        assertRuntimeComparisons(source, output, caseInsensitive);
    }

    private void assertComparison(String source, String enumName, String expectedComparison) {
        String method = extractFromValue(source, enumName);
        assertTrue(method.contains("if (" + expectedComparison + ")"),
                enumName + ".fromValue should use " + expectedComparison + ":\n" + method);
    }

    private String extractFromValue(String source, String enumName) {
        String signature = "public static " + enumName + " fromValue(";
        int methodStart = source.indexOf(signature);
        assertTrue(methodStart >= 0, "Missing method: " + signature);
        int bodyStart = source.indexOf('{', methodStart);
        assertTrue(bodyStart > methodStart, "Missing method body: " + signature);
        int depth = 1;
        int end = bodyStart + 1;
        while (end < source.length() && depth > 0) {
            char character = source.charAt(end++);
            if (character == '{') {
                depth++;
            } else if (character == '}') {
                depth--;
            }
        }
        assertEquals(depth, 0, "Unclosed method: " + signature);
        return source.substring(methodStart, end);
    }

    private void assertRuntimeComparisons(String source, Path output, boolean caseInsensitive) throws Exception {
        // Compile the actual generated methods in minimal enums, without unrelated serializer dependencies.
        // Columns: enum name, Java type, constant expression, exact value, alternate case, nullable.
        Object[][] cases = {
                {"PlainStringEnum", String.class, "\"FIRST\"", "FIRST", "first", false},
                {"FormattedStringEnum", String.class, "\"FIRST\"", "FIRST", "first", false},
                {"EmailEnum", String.class, "\"support@example.com\"", "support@example.com", "SUPPORT@example.com", false},
                {"NullableStringEnum", String.class, "\"null\"", "null", "NULL", true},
                {"UuidEnum", UUID.class, "UUID.fromString(\"123e4567-e89b-12d3-a456-426614174000\")",
                        UUID.fromString("123e4567-e89b-12d3-a456-426614174000"), null, false},
                {"NumberEnum", Integer.class, "1", 1, null, false},
                {"FormattedNumberEnum", Long.class, "1L", 1L, null, false},
                {"ExampleInnerThreeEnum", URI.class, "URI.create(\"https://example.com/one\")",
                        URI.create("https://example.com/one"), URI.create("https://example.com/ONE"), false},
                {"NullableUriEnum", URI.class, "URI.create(\"https://example.com/one\")",
                        URI.create("https://example.com/one"), URI.create("https://example.com/ONE"), true}
        };
        StringBuilder fixture = new StringBuilder("import java.net.URI;\nimport java.util.UUID;\npublic class EnumFixture {\n");
        for (Object[] testCase : cases) {
            String name = (String) testCase[0];
            String type = ((Class<?>) testCase[1]).getSimpleName();
            fixture.append("public enum ").append(name).append(" { FIRST(").append(testCase[2]).append(");\n")
                    .append("private final ").append(type).append(" value;\n")
                    .append(name).append('(').append(type).append(" value) { this.value = value; }\n")
                    .append(extractFromValue(source, name)).append("\n}\n");
        }
        fixture.append("}\n");
        Path runtime = Files.createDirectory(output.resolve("runtime"));
        Path fixtureFile = runtime.resolve("EnumFixture.java");
        Files.writeString(fixtureFile, fixture);
        ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
        assertNotNull(ToolProvider.getSystemJavaCompiler(), "Runtime regression tests require a JDK");
        int result = ToolProvider.getSystemJavaCompiler().run(null, diagnostics, diagnostics,
                "-d", runtime.toString(), fixtureFile.toString());
        assertEquals(result, 0, diagnostics.toString(StandardCharsets.UTF_8));

        try (URLClassLoader loader = new URLClassLoader(new URL[]{runtime.toUri().toURL()}, null)) {
            for (Object[] testCase : cases) {
                Class<?> enumClass = loader.loadClass("EnumFixture$" + testCase[0]);
                Method fromValue = enumClass.getMethod("fromValue", (Class<?>) testCase[1]);
                Object expected = enumClass.getEnumConstants()[0];
                boolean nullable = (boolean) testCase[5];
                assertSame(fromValue.invoke(null, testCase[3]), expected);
                assertUnmatched(fromValue, null, nullable);
                if (testCase[4] != null) {
                    if (caseInsensitive) {
                        assertSame(fromValue.invoke(null, testCase[4]), expected);
                    } else {
                        assertUnmatched(fromValue, testCase[4], nullable);
                    }
                }
            }
        }
    }

    private void assertUnmatched(Method fromValue, Object value, boolean nullable) throws Exception {
        if (nullable) {
            assertNull(fromValue.invoke(null, value));
        } else {
            InvocationTargetException exception = expectThrows(InvocationTargetException.class,
                    () -> fromValue.invoke(null, value));
            assertTrue(exception.getCause() instanceof IllegalArgumentException,
                    fromValue + " should reject unmatched input with IllegalArgumentException: " + exception.getCause());
        }
    }
}
