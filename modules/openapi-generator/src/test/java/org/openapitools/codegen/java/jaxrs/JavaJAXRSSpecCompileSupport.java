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

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.GlobalSettings;
import org.testng.SkipException;

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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.TestUtils.newTempFolder;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

/**
 * Generates jaxrs-spec models, compiles them with the system Java compiler and loads them, for the tests that bind the
 * generated models at runtime.
 */
final class JavaJAXRSSpecCompileSupport {

    static final String SPEC_DIR = "src/test/resources/3_0/jaxrs-spec/";
    static final String MODEL_PACKAGE = "org.openapitools.model";
    private static final Pattern GENERATED_ANNOTATION = Pattern.compile("(?m)^@(jakarta|javax)\\.annotation\\.Generated\\(.*\\)\\R");

    private JavaJAXRSSpecCompileSupport() {
    }

    /**
     * Generates the models of the spec into a new temporary folder.
     */
    static Path generate(String spec, Map<String, Object> properties) throws Exception {
        Path output = newTempFolder();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setInputSpec(spec)
                .setOutputDir(output.toString())
                .addGlobalProperty("models", "")
                .addGlobalProperty("modelDocs", "false")
                .addGlobalProperty("modelTests", "false");
        properties.forEach(configurator::addAdditionalProperty);
        try {
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        } finally {
            // the global properties are thread-local and would leak into the next test class on this thread
            GlobalSettings.reset();
        }
        return output;
    }

    static Path modelDir(Path output) {
        return output.resolve("src/gen/java/org/openapitools/model");
    }

    static String withoutGeneratedAnnotation(Path source) throws Exception {
        return GENERATED_ANNOTATION.matcher(Files.readString(source)).replaceAll("");
    }

    /**
     * Compiles the generated models for Java 17 as generated; only {@code @Generated} is dropped (jakarta.annotation is
     * not on the test classpath).
     *
     * @return the directory of the compiled classes
     */
    static Path compileModels(Path output, String classpath) throws Exception {
        Path sources = Files.createDirectories(output.resolve("compile-sources"));
        try (Stream<Path> models = Files.list(modelDir(output))) {
            for (Path model : models.filter(p -> p.toString().endsWith(".java")).collect(Collectors.toList())) {
                Files.writeString(sources.resolve(model.getFileName()), withoutGeneratedAnnotation(model));
            }
        }
        Path classes = output.resolve("classes");
        CompileResult result = compile(sources, classes, "17", classpath);
        assertEquals(result.exitCode, 0, result.diagnostics);
        return classes;
    }

    /**
     * Compiles the sources against the given classes (for example the compiled models) and classpath into the classes.
     */
    static void assertCompiles(Path sources, Path classes, String release, String classpath) throws Exception {
        CompileResult result = compile(sources, classes, release, classes + File.pathSeparator + classpath);
        assertEquals(result.exitCode, 0, result.diagnostics);
    }

    static CompileResult compile(Path sources, Path classes, String release, String classpath) throws Exception {
        Files.createDirectories(classes);
        List<String> args = new ArrayList<>(Arrays.asList("--release", release, "-proc:none", "-classpath", classpath, "-d", classes.toString()));
        try (Stream<Path> files = Files.list(sources)) {
            files.filter(p -> p.toString().endsWith(".java")).map(Path::toString).forEach(args::add);
        }
        ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
        assertNotNull(ToolProvider.getSystemJavaCompiler(), "Compile tests require a JDK");
        int exitCode = ToolProvider.getSystemJavaCompiler().run(null, diagnostics, diagnostics, args.toArray(new String[0]));
        return new CompileResult(exitCode, diagnostics.toString(StandardCharsets.UTF_8));
    }

    /**
     * Writes a source file of the given lines into the directory and returns the directory.
     */
    static Path writeSource(Path directory, String className, String... lines) throws Exception {
        Files.createDirectories(directory);
        Files.writeString(directory.resolve(className + ".java"), String.join("\n", lines) + "\n");
        return directory;
    }

    static URLClassLoader loader(Path classes) throws Exception {
        return new URLClassLoader(new URL[]{classes.toUri().toURL()}, JavaJAXRSSpecCompileSupport.class.getClassLoader());
    }

    static void requireJdk(int feature) {
        if (Runtime.version().feature() < feature) {
            throw new SkipException("Requires JDK " + feature + "+, running on " + Runtime.version());
        }
    }

    /**
     * The classpath of the jars (or directories) that contain the given classes.
     */
    static String classpath(Class<?>... types) throws Exception {
        List<String> entries = new ArrayList<>();
        for (Class<?> type : types) {
            entries.add(Paths.get(type.getProtectionDomain().getCodeSource().getLocation().toURI()).toString());
        }
        return String.join(File.pathSeparator, entries);
    }

    static final class CompileResult {
        final int exitCode;
        final String diagnostics;

        private CompileResult(int exitCode, String diagnostics) {
            this.exitCode = exitCode;
            this.diagnostics = diagnostics;
        }
    }
}
