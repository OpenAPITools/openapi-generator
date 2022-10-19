/*
 * Copyright 2022 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright (c) 2022 Oracle and/or its affiliates
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

package org.openapitools.codegen.java.helidon.functional;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JavaHelidonCommonCodegen;
import org.testng.SkipException;

import static java.util.Objects.requireNonNull;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

abstract class FunctionalBase {

    private static final Logger LOGGER = Logger.getLogger(FunctionalBase.class.getName());

    private static final String MAVEN_SHIM_TARGET = "libexec/bin/mvn";
    private static final String MAVEN_HOME_VAR = "MAVEN_HOME";
    private static final String MVN_HOME_VAR = "MVN_HOME";
    private static final String PATH_VAR = "PATH";
    private static final String MAVEN_BINARY_NAME;
    private static final boolean IS_WINDOWS_OS;
    private static final List<Map.Entry<Integer, String>> DEFAULT_HELIDON_VERSIONS_FOR_JAVA_VERSIONS = new ArrayList<>();

    protected static final String FULL_PROJECT = "fullProject";
    protected static final String USE_ABSTRACT_CLASS = "useAbstractClass";

    static {
        /*
            The inferred Helidon version for tests is from the entry for which the Java major version does not exceed
            the current runtime Java major version.

            For example, for Java 8 or 9 or 11: 2.5.3. For Java 13 or later: 3.0.1.
         */
        DEFAULT_HELIDON_VERSIONS_FOR_JAVA_VERSIONS.add(new AbstractMap.SimpleEntry<>(11, "2.5.3"));
        DEFAULT_HELIDON_VERSIONS_FOR_JAVA_VERSIONS.add(new AbstractMap.SimpleEntry<>(13, "3.0.1"));
    }

    private String library;
    private String generatorName;
    private String inputSpec;
    protected Path outputPath;

    private Path mvn;

    static {
        IS_WINDOWS_OS = System.getProperty("os.name", "unknown")
                .toLowerCase(Locale.ENGLISH)
                .contains("win");
        MAVEN_BINARY_NAME = IS_WINDOWS_OS ? "mvn.cmd" : "mvn";
    }

    protected CodegenConfigurator createConfigurator() {
        try {
           return createConfigurator(Files.createTempDirectory("test"));
        } catch (IOException e) {
            throw new UncheckedIOException("Can not create temp directory", e);
        }
    }

    protected CodegenConfigurator createConfigurator(Path outputPath) {
        Objects.requireNonNull(inputSpec);
        this.outputPath = outputPath;
        String sanitizedPath = outputPath.toFile()
                .getAbsolutePath()
                .replace('\\', '/');
        return new CodegenConfigurator()
                .setGeneratorName(generatorName)
                .setLibrary(library)
                .setInputSpec(inputSpec)
                .setOutputDir(sanitizedPath);

    }

    protected void generate(CodegenConfigurator config) {
        String helidonVersionToUse = chooseHelidonVersion(config);
        enforceJavaVersion(helidonVersionToUse);
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(config.toClientOptInput());
        generator.generate();
    }

    protected void generate() {
        generate(createConfigurator());
    }

    protected void generate(String inputSpec) {
        inputSpec(inputSpec);
        generate(createConfigurator());
    }

    protected void generatorName(String generatorName) {
        this.generatorName = generatorName;
    }

    protected void library(String library) {
        this.library = library;
    }

    protected void inputSpec(String inputSpec) {
        this.inputSpec = inputSpec;
    }

    /**
     * Run maven command with provided arguments.
     *
     * @param args maven command arguments
     * @return a {@link ProcessReader}
     */
    protected ProcessReader runMavenProcess(String... args) {
        return runMavenProcess(outputPath.toFile(), args);
    }

    /**
     * Run maven command and causes the current thread to wait for {@link Process} to terminate.
     *
     * @param args maven command arguments
     * @return a {@link ProcessReader}
     */
    protected ProcessReader runMavenProcessAndWait(String... args) {
        ProcessReader process = runMavenProcess(args);
        process.waitFor(10, TimeUnit.MINUTES);
        return process;
    }

    /**
     * Run maven command in the provided directory.
     *
     * @param directory from where the command is executed
     * @param args  maven command arguments
     * @return a {@link ProcessReader}
     */
    protected ProcessReader runMavenProcess(File directory, String... args) {
        List<String> command = new ArrayList<>(Collections.singleton(mavenExecutable()));
        Collections.addAll(command, args);
        try {
            Process process = new ProcessBuilder()
                    .directory(directory)
                    .command(command)
                    .start();
            return new ProcessReader(process);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * Finds the {@code mvn} executable. Searches using the following, in order:
     * <ol>
     *     <li>The {@code MAVEN_HOME} environment variable</li>
     *     <li>The {@code MVN_HOME} environment variable</li>
     *     <li>The {@code PATH} environment variable</li>
     * </ol>
     *
     * @return The path.
     */
    public String mavenExecutable() {
        if (mvn == null) {
            Path maven;
            Optional<Path> path = findExecutableInPath();
            if (path.isPresent()) {
                maven = path.get();
            } else {
                maven = toMavenExecutable(MAVEN_HOME_VAR);
                if (maven == null) {
                    maven = toMavenExecutable(MVN_HOME_VAR);
                }
            }
            try {
                assumeTrue( "Maven not found, test is skipped", maven != null);
                maven = maven.toRealPath();
                Path shimmed = maven.getParent().getParent().resolve(MAVEN_SHIM_TARGET);
                if (Files.exists(shimmed)) {
                    maven = shimmed;
                }
                mvn = maven.toRealPath();
            } catch (IOException ex) {
                throw new IllegalStateException(ex.getMessage());
            }
        }
        return mvn.toString();
    }

    private String chooseHelidonVersion(CodegenConfigurator config) {
        Map<String, Object> unprocessedAdditionalProperties = config.toContext()
            .getGeneratorSettings()
            .getAdditionalProperties();
        if (unprocessedAdditionalProperties.containsKey(JavaHelidonCommonCodegen.HELIDON_VERSION)) {
            return unprocessedAdditionalProperties.get(JavaHelidonCommonCodegen.HELIDON_VERSION).toString();
        }
        String result = inferredHelidonVersion();
        config.addAdditionalProperty(JavaHelidonCommonCodegen.HELIDON_VERSION, result);
        return result;
    }

    private void enforceJavaVersion(String helidonVersionToUse) {
        int currentJavaVersion = getCurrentJavaMajorVersion();
        int requiredJavaVersion = getRequiredJavaVersion(helidonVersionToUse);
        String errorJavaVersion = String.format(Locale.ROOT, "Java version must be %s, test is skipped", requiredJavaVersion);
        assumeTrue(errorJavaVersion, currentJavaVersion == requiredJavaVersion);
    }

    private int getRequiredJavaVersion(String helidonVersionToUse) {
        return helidonVersionToUse
                .startsWith("3.") ? 17 : 11;
    }

    private int getCurrentJavaMajorVersion() {
        String[] versionElements = System.getProperty("java.version").split("\\.");
        int firstElement = Integer.parseInt(versionElements[0]);
        if (firstElement == 1) {
            return Integer.parseInt(versionElements[1]);
        } else {
            return firstElement;
        }
    }

    private String inferredHelidonVersion() {
        int javaMajorVersion = getCurrentJavaMajorVersion();
        String result = null;
        for (Map.Entry<Integer, String> javaToHelidonVersionMapping : DEFAULT_HELIDON_VERSIONS_FOR_JAVA_VERSIONS) {
            if (javaToHelidonVersionMapping.getKey() <= javaMajorVersion) {
                result = javaToHelidonVersionMapping.getValue();
            }
        }
        if (result == null) {
            String message = String.format(Locale.ROOT, "Unable to infer Helidon version from current Java major version %d using mapping %s",
                javaMajorVersion, DEFAULT_HELIDON_VERSIONS_FOR_JAVA_VERSIONS);
            LOGGER.log(Level.WARNING, message);
            throw new SkipException(message);
        }
        return result;
    }

    /**
     * Find an executable in the {@code PATH} environment variable, if present.
     *
     * @return The path.
     */
    private Optional<Path> findExecutableInPath() {
        return Arrays.stream(requireNonNull(System.getenv(PATH_VAR)).split(File.pathSeparator))
                .map(Paths::get)
                .map(path -> path.resolve(FunctionalBase.MAVEN_BINARY_NAME))
                .filter(Files::isExecutable)
                .findFirst();
    }

    private Path toMavenExecutable(String mavenHomeEnvVar) {
        Path mavenHome = envVarPath(mavenHomeEnvVar);
        if (mavenHome != null) {
            if (Files.isDirectory(mavenHome)) {
                Path executable = mavenHome.resolve("bin").resolve(MAVEN_BINARY_NAME);
                if (Files.exists(executable) && (IS_WINDOWS_OS || Files.isExecutable(executable))) {
                    return executable;
                }
            }
        }
        return null;
    }

    private static Path envVarPath(String var) {
        final String path = System.getenv(var);
        return path == null ? null : Paths.get(path);
    }

    /**
     * Allow junit to skip test without throwing an exception and report tests as failed.
     *
     * @param message warning message
     * @param condition to be checked
     */
    protected static void assumeTrue(String message, boolean condition) {
        if (!condition) {
            LOGGER.log(Level.WARNING, message);
            throw new SkipException(message);
        }
    }

    /**
     * Convenience method to build project using Maven and verify test output.
     *
     * @param jarPath path to expected jar file
     */
    protected void buildAndVerify(String jarPath) {
        ProcessReader reader = runMavenProcessAndWait("package");
        Path executableJar = outputPath.resolve(jarPath);
        String output = reader.readOutputConsole();
        assertThat(output, containsString("BUILD SUCCESS"));
        assertThat(output, containsString("Errors: 0"));
        assertThat(output, containsString("Failures: 0"));
        assertThat(output, containsString("Skipped: 0"));
        assertThat(Files.exists(executableJar), is(true));
    }

    /**
     * {@link Process} wrapper to read I/O Stream.
     */
    static class ProcessReader {

        private final Process process;
        private final BufferedReader consoleReader;

        ProcessReader(Process process) {
            this.process = process;
            this.consoleReader = new BufferedReader(new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8));
        }

        public String readOutputConsole() {
            return consoleReader.lines().collect(Collectors.joining("\n"));
        }

        @SuppressWarnings("UnusedReturnValue")
        public boolean waitFor(long timeout, TimeUnit unit) {
            try {
                return process.waitFor(timeout, unit);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
