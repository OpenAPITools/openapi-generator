/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen;

import org.testng.SkipException;
import org.testng.annotations.Test;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.stream.Stream;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.expectThrows;

public class CodegenConfigLoaderTest {

    @Test
    public void testConfigClassLoadsFromContextClassLoaderWhenNotOnDefaultClassLoader() throws Exception {
        Path classesDir = Files.createTempDirectory("codegen-config-tccl-test");
        try {
            String className = "org.openapitools.codegen.testfixture.TcclOnlyCodegen";
            compileCodegenFixture(classesDir, className);
            Path serviceFile = classesDir.resolve("META-INF/services/" + CodegenConfig.class.getName());
            Files.createDirectories(serviceFile.getParent());
            Files.writeString(serviceFile, className);

            ClassLoader originalTccl = Thread.currentThread().getContextClassLoader();
            URLClassLoader isolatedLoader = new URLClassLoader(
                    new URL[]{classesDir.toUri().toURL()}, originalTccl);
            try {
                Thread.currentThread().setContextClassLoader(isolatedLoader);

                CodegenConfig config = CodegenConfigLoader.forName("tccl-only-codegen");
                CodegenConfig configByClassName = CodegenConfigLoader.forName(className);

                assertEquals(config.getClass().getName(), className);
                assertEquals(config.getClass().getClassLoader(), isolatedLoader);
                assertEquals(configByClassName.getClass().getClassLoader(), isolatedLoader);
            } finally {
                Thread.currentThread().setContextClassLoader(originalTccl);
                isolatedLoader.close();
            }
        } finally {
            deleteRecursively(classesDir);
        }
    }

    @Test
    public void testConfigClassFallsBackToDefaultClassLoaderWhenContextClassLoaderIsNull() {
        ClassLoader originalTccl = Thread.currentThread().getContextClassLoader();
        try {
            Thread.currentThread().setContextClassLoader(null);

            CodegenConfig config = CodegenConfigLoader.forName(DefaultCodegen.class.getName());

            assertEquals(config.getClass(), DefaultCodegen.class);
        } finally {
            Thread.currentThread().setContextClassLoader(originalTccl);
        }
    }

    @Test
    public void testConfigClassFallsBackWhenContextClassLoaderCannotResolveClass() throws Exception {
        ClassLoader originalTccl = Thread.currentThread().getContextClassLoader();
        URLClassLoader isolatedLoader = new URLClassLoader(new URL[0], null);
        try {
            Thread.currentThread().setContextClassLoader(isolatedLoader);

            CodegenConfig config = CodegenConfigLoader.forName(DefaultCodegen.class.getName());

            assertEquals(config.getClass(), DefaultCodegen.class);
        } finally {
            Thread.currentThread().setContextClassLoader(originalTccl);
            isolatedLoader.close();
        }
    }

    @Test
    public void testConfigClassNotFoundProducesClearErrorMessage() {
        GeneratorNotFoundException exception = expectThrows(GeneratorNotFoundException.class,
                () -> CodegenConfigLoader.forName("does.not.Exist"));

        assertTrue(exception.getMessage().contains("does.not.Exist"));
        assertTrue(exception.getMessage().contains("classpath"));
        assertTrue(exception.getMessage().contains("Available:"));
    }

    private static void compileCodegenFixture(Path outputDir, String fullyQualifiedClassName) throws Exception {
        int lastDot = fullyQualifiedClassName.lastIndexOf('.');
        String packageName = fullyQualifiedClassName.substring(0, lastDot);
        String simpleName = fullyQualifiedClassName.substring(lastDot + 1);
        Path sourceDir = Files.createTempDirectory("codegen-config-tccl-src");
        try {
            Path packageDir = sourceDir.resolve(packageName.replace('.', File.separatorChar));
            Files.createDirectories(packageDir);
            Path sourceFile = packageDir.resolve(simpleName + ".java");
            Files.writeString(sourceFile, "package " + packageName + ";\n"
                    + "public class " + simpleName + " extends org.openapitools.codegen.DefaultCodegen {\n"
                    + "    public " + simpleName + "() {}\n"
                    + "    @Override public String getName() { return \"tccl-only-codegen\"; }\n"
                    + "}\n");

            JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
            if (compiler == null) {
                throw new SkipException("No system Java compiler available (test requires a JDK, not a JRE)");
            }
            int result = compiler.run(null, null, null,
                    "-d", outputDir.toString(),
                    "-cp", System.getProperty("java.class.path"),
                    sourceFile.toString());
            assertEquals(result, 0, "Failed to compile test fixture generator class");
        } finally {
            deleteRecursively(sourceDir);
        }
    }

    private static void deleteRecursively(Path root) throws IOException {
        if (!Files.exists(root)) {
            return;
        }
        try (Stream<Path> paths = Files.walk(root)) {
            paths.sorted(Comparator.reverseOrder())
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (IOException ignored) {
                            // best-effort cleanup
                        }
                    });
        }
    }
}
