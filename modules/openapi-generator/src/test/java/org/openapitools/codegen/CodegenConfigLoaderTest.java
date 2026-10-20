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

import static org.testng.Assert.*;

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
            try (URLClassLoader isolatedLoader = new URLClassLoader(
                    new URL[]{classesDir.toUri().toURL()}, originalTccl)) {
                Thread.currentThread().setContextClassLoader(isolatedLoader);

                CodegenConfig config = CodegenConfigLoader.forName("tccl-only-codegen");
                CodegenConfig configByClassName = CodegenConfigLoader.forName(className);

                assertEquals(config.getClass().getName(), className);
                assertEquals(config.getClass().getClassLoader(), isolatedLoader);
                assertEquals(configByClassName.getClass().getClassLoader(), isolatedLoader);
            } finally {
                Thread.currentThread().setContextClassLoader(originalTccl);
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
        try (URLClassLoader isolatedLoader = new URLClassLoader(new URL[0], null)) {
            Thread.currentThread().setContextClassLoader(isolatedLoader);

            CodegenConfig config = CodegenConfigLoader.forName("java");
            CodegenConfig configByClassName = CodegenConfigLoader.forName(DefaultCodegen.class.getName());

            assertEquals(config.getName(), "java");
            assertEquals(configByClassName.getClass(), DefaultCodegen.class);
            assertTrue(CodegenConfigLoader.getAll().stream().anyMatch(candidate -> "java".equals(candidate.getName())));
        } finally {
            Thread.currentThread().setContextClassLoader(originalTccl);
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

    @Test
    public void testFoundClassThatDoesNotImplementCodegenConfigProducesClearErrorMessage() {
        GeneratorNotFoundException exception = expectThrows(GeneratorNotFoundException.class,
                () -> CodegenConfigLoader.forName(String.class.getName()));

        assertTrue(exception.getMessage().contains(String.class.getName()));
        assertTrue(exception.getMessage().contains("found but could not be constructed"));
        assertTrue(exception.getMessage().contains("implement CodegenConfig"));
    }

    @Test
    public void testFoundConfigClassWithoutPublicNoArgConstructorProducesClearErrorMessage() throws Exception {
        Path classesDir = Files.createTempDirectory("codegen-config-constructor-test");
        try {
            String className = "org.openapitools.codegen.testfixture.PrivateConstructorCodegen";
            compileCodegenFixture(classesDir, className, false, "private-constructor-codegen");

            ClassLoader originalTccl = Thread.currentThread().getContextClassLoader();
            try (URLClassLoader isolatedLoader = new URLClassLoader(
                    new URL[]{classesDir.toUri().toURL()}, originalTccl)) {
                Thread.currentThread().setContextClassLoader(isolatedLoader);

                GeneratorNotFoundException exception = expectThrows(GeneratorNotFoundException.class,
                        () -> CodegenConfigLoader.forName(className));

                assertTrue(exception.getMessage().contains(className));
                assertTrue(exception.getMessage().contains("found but could not be constructed"));
                assertTrue(exception.getMessage().contains("public no-argument constructor"));
            } finally {
                Thread.currentThread().setContextClassLoader(originalTccl);
            }
        } finally {
            deleteRecursively(classesDir);
        }
    }

    @Test
    public void testConfigClassWithLinkageErrorProducesClasspathGuidance() throws Exception {
        Path classesDir = Files.createTempDirectory("codegen-config-linkage-test");
        try {
            String className = "org.openapitools.codegen.testfixture.LinkageErrorCodegen";
            compileCodegenFixture(classesDir, className, true, "linkage-error-codegen",
                    "    static { if (System.nanoTime() >= 0) throw new NoClassDefFoundError(\"missing dependency\"); }\n");

            ClassLoader originalTccl = Thread.currentThread().getContextClassLoader();
            try (URLClassLoader isolatedLoader = new URLClassLoader(
                    new URL[]{classesDir.toUri().toURL()}, originalTccl)) {
                Thread.currentThread().setContextClassLoader(isolatedLoader);

                GeneratorNotFoundException exception = expectThrows(GeneratorNotFoundException.class,
                        () -> CodegenConfigLoader.forName(className));

                assertTrue(exception.getMessage().contains(className));
                assertTrue(exception.getMessage().contains("classpath"));
                assertTrue(exception.getCause() instanceof NoClassDefFoundError);
            } finally {
                Thread.currentThread().setContextClassLoader(originalTccl);
            }
        } finally {
            deleteRecursively(classesDir);
        }
    }

    @Test
    public void testUnrelatedBrokenSpiProviderDoesNotPreventDirectConfigLoading() throws Exception {
        Path classesDir = Files.createTempDirectory("codegen-config-broken-spi-test");
        try {
            String className = "org.openapitools.codegen.testfixture.BrokenSpiCodegen";
            compileCodegenFixture(classesDir, className, true, "broken-spi-codegen",
                    "    static { if (System.nanoTime() >= 0) throw new IllegalStateException(\"fixture failure\"); }\n");
            Path serviceFile = classesDir.resolve("META-INF/services/" + CodegenConfig.class.getName());
            Files.createDirectories(serviceFile.getParent());
            Files.writeString(serviceFile, className);

            ClassLoader originalTccl = Thread.currentThread().getContextClassLoader();
            try (URLClassLoader isolatedLoader = new URLClassLoader(
                    new URL[]{classesDir.toUri().toURL()}, originalTccl)) {
                Thread.currentThread().setContextClassLoader(isolatedLoader);

                CodegenConfig config = CodegenConfigLoader.forName(DefaultCodegen.class.getName());

                assertEquals(config.getClass(), DefaultCodegen.class);
            } finally {
                Thread.currentThread().setContextClassLoader(originalTccl);
            }
        } finally {
            deleteRecursively(classesDir);
        }
    }

    @Test
    public void testMalformedSpiEntryDoesNotPreventDirectConfigLoading() throws Exception {
        Path classesDir = Files.createTempDirectory("codegen-config-malformed-spi-test");
        try {
            Path serviceFile = classesDir.resolve("META-INF/services/" + CodegenConfig.class.getName());
            Files.createDirectories(serviceFile.getParent());
            Files.writeString(serviceFile, "org.openapitools.codegen.testfixture.DoesNotExist");

            ClassLoader originalTccl = Thread.currentThread().getContextClassLoader();
            try (URLClassLoader isolatedLoader = new URLClassLoader(
                    new URL[]{classesDir.toUri().toURL()}, originalTccl)) {
                Thread.currentThread().setContextClassLoader(isolatedLoader);

                CodegenConfig config = CodegenConfigLoader.forName(DefaultCodegen.class.getName());

                assertEquals(config.getClass(), DefaultCodegen.class);
            } finally {
                Thread.currentThread().setContextClassLoader(originalTccl);
            }
        } finally {
            deleteRecursively(classesDir);
        }
    }

    @Test
    public void testConfigClassWithFailingStaticInitializerProducesPreciseErrorMessage() throws Exception {
        Path classesDir = Files.createTempDirectory("codegen-config-initializer-test");
        try {
            String className = "org.openapitools.codegen.testfixture.InitializerErrorCodegen";
            compileCodegenFixture(classesDir, className, true, "initializer-error-codegen",
                    "    static { if (System.nanoTime() >= 0) throw new IllegalStateException(\"fixture failure\"); }\n");

            ClassLoader originalTccl = Thread.currentThread().getContextClassLoader();
            try (URLClassLoader isolatedLoader = new URLClassLoader(
                    new URL[]{classesDir.toUri().toURL()}, originalTccl)) {
                Thread.currentThread().setContextClassLoader(isolatedLoader);

                GeneratorNotFoundException exception = expectThrows(GeneratorNotFoundException.class,
                        () -> CodegenConfigLoader.forName(className));

                assertTrue(exception.getMessage().contains(className));
                assertTrue(exception.getMessage().contains("static initializer failed"));
                assertFalse(exception.getMessage().contains("classpath"));
                assertTrue(exception.getCause() instanceof ExceptionInInitializerError);

                GeneratorNotFoundException retryException = expectThrows(GeneratorNotFoundException.class,
                        () -> CodegenConfigLoader.forName(className));

                assertTrue(retryException.getMessage().contains(className));
                assertTrue(retryException.getMessage().contains("static initializer failed"));
                assertFalse(retryException.getMessage().contains("classpath"));
                assertTrue(retryException.getCause() instanceof NoClassDefFoundError);
            } finally {
                Thread.currentThread().setContextClassLoader(originalTccl);
            }
        } finally {
            deleteRecursively(classesDir);
        }
    }

    private static void compileCodegenFixture(Path outputDir, String fullyQualifiedClassName) throws Exception {
        compileCodegenFixture(outputDir, fullyQualifiedClassName, true, "tccl-only-codegen", "");
    }

    private static void compileCodegenFixture(Path outputDir, String fullyQualifiedClassName,
                                              boolean publicNoArgConstructor, String generatorName) throws Exception {
        compileCodegenFixture(outputDir, fullyQualifiedClassName, publicNoArgConstructor, generatorName, "");
    }

    private static void compileCodegenFixture(Path outputDir, String fullyQualifiedClassName,
                                              boolean publicNoArgConstructor, String generatorName,
                                              String staticInitializer) throws Exception {
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
                    + "    " + (publicNoArgConstructor ? "public" : "private") + " " + simpleName + "() {}\n"
                    + staticInitializer
                    + "    @Override public String getName() { return \"" + generatorName + "\"; }\n"
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
