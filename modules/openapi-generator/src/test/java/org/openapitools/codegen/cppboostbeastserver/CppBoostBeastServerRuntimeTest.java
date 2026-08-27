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

package org.openapitools.codegen.cppboostbeastserver;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

/**
 * Generates a server from the OAS 3.1 regression spec, compiles it together
 * with the loopback driver, runs it against real sockets, and asserts the
 * sentinel output. This is the end-to-end behavior proof for the generator:
 * routing (including literal-over-parameter ranking), parameter codecs
 * (matrix explode, spaceDelimited, deepObject), body decoding, security
 * challenges, version mirroring, deferred-response timers, and error mapping
 * all execute in the produced C++ binary.
 */
public class CppBoostBeastServerRuntimeTest {

    private static final String SPEC =
            "src/test/resources/3_1/cpp-boost-beast-server/server-regression.yaml";
    private static final String DRIVER =
            "src/test/resources/3_1/cpp-boost-beast-server/"
                    + "server-runtime-regression.cpp";

    @Test
    public void generatedServerServesLoopbackRegressions() throws Exception {
        Path output = generateServer(Map.of());
        Assert.assertTrue(
                compileAndRunDriver(output).contains(
                        "cpp-boost-beast-server runtime regressions passed"),
                "server runtime test did not report completion");
    }

    @Test
    public void validationDisabledOutputCompilesAndServesLoopbackRegressions()
            throws Exception {
        Path output = generateServer(
                Map.of("compileWithValidation", Boolean.FALSE));
        Assert.assertTrue(
                compileAndRunDriver(output).contains(
                        "cpp-boost-beast-server runtime regressions passed"),
                "validation-disabled server runtime test did not complete");
    }

    private static Path generateServer(Map<String, Object> properties)
            throws IOException {
        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(
                outputRoot, "cpp-boost-beast-server-runtime-");
        output.toFile().deleteOnExit();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-server")
                .setInputSpec(SPEC)
                .setOutputDir(output.toString());
        properties.forEach(configurator::addAdditionalProperty);
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        return output;
    }

    private static String compileAndRunDriver(Path output) throws Exception {
        Path executable = output.resolve("server-runtime-regression");
        String compiler = System.getenv().getOrDefault("CXX", "c++");

        List<String> command = new ArrayList<>();
        command.add(compiler);
        command.add("-std=c++17");
        command.add("-Wall");
        command.add("-Werror");
        command.add("-DBOOST_ERROR_CODE_HEADER_ONLY");
        command.add("-I" + output);
        command.add("-I" + output.resolve("api"));
        command.add("-I" + output.resolve("model"));
        command.add("-I" + output.resolve("server"));
        for (String candidate : new String[]{"/opt/homebrew", "/usr/local"}) {
            Path include = Path.of(candidate, "include");
            Path lib = Path.of(candidate, "lib");
            if (Files.isDirectory(include)) {
                command.add("-I" + include);
            }
            if (Files.isDirectory(lib)) {
                command.add("-L" + lib);
            }
        }
        command.add(Path.of(DRIVER).toString());
        try (Stream<Path> sources = Files.list(output.resolve("model"))) {
            sources.filter(path -> path.getFileName().toString().endsWith(".cpp"))
                    .map(Path::toString)
                    .sorted()
                    .forEach(command::add);
        }
        try (Stream<Path> sources = Files.list(output.resolve("api"))) {
            sources.filter(path -> path.getFileName().toString().endsWith(".cpp"))
                    .map(Path::toString)
                    .sorted()
                    .forEach(command::add);
        }
        command.add(output.resolve("server/HttpServer.cpp").toString());
        command.add("-lboost_json");
        command.add("-lboost_url");
        command.add("-pthread");
        command.add("-o");
        command.add(executable.toString());

        // Redirect to files instead of pipes: a chatty compiler would
        // otherwise deadlock the pipe buffer while waitFor() blocks.
        Path compileLog = output.resolve("compile.log");
        Process compile;
        try {
            compile = new ProcessBuilder(command)
                    .redirectErrorStream(true)
                    .redirectOutput(compileLog.toFile())
                    .directory(new File("."))
                    .start();
        } catch (IOException unavailable) {
            throw unavailableDependency("C++ compiler is unavailable (" + compiler + ")",
                    unavailable.getMessage());
        }
        if (!compile.waitFor(10, TimeUnit.MINUTES)) {
            terminate(compile);
            Assert.fail("server runtime compile timed out:\n"
                    + readQuietly(compileLog));
        }
        String compileOutput = readQuietly(compileLog);
        if (compile.exitValue() != 0 && missingBoost(compileOutput)) {
            throw unavailableDependency("Boost development files are unavailable",
                    compileOutput.trim());
        }
        Assert.assertEquals(compile.exitValue(), 0,
                "server runtime compile failed:\n" + compileOutput);

        Path runLog = output.resolve("run.log");
        Process run;
        try {
            run = new ProcessBuilder(executable.toString())
                    .redirectErrorStream(true)
                    .redirectOutput(runLog.toFile())
                    .start();
        } catch (IOException unavailable) {
            throw unavailableDependency("compiled binary could not start",
                    unavailable.getMessage());
        }
        if (!run.waitFor(120, TimeUnit.SECONDS)) {
            terminate(run);
            Assert.fail("server runtime execution timed out:\n"
                    + readQuietly(runLog));
        }
        String runOutput = readQuietly(runLog);
        Assert.assertEquals(run.exitValue(), 0,
                "server runtime test failed:\n" + runOutput);
        return runOutput;
    }

    private static void terminate(Process process) {
        process.descendants().forEach(ProcessHandle::destroyForcibly);
        process.destroyForcibly();
    }

    private static String readQuietly(Path log) {
        try {
            return Files.readString(log, StandardCharsets.UTF_8);
        } catch (IOException missing) {
            return "<no log captured: " + missing.getMessage() + ">";
        }
    }

    private static boolean missingBoost(String compilerOutput) {
        String normalized = compilerOutput.toLowerCase(java.util.Locale.ROOT);
        boolean missingHeaders = normalized.contains("boost/")
                && (normalized.contains("not found")
                || normalized.contains("no such file"));
        boolean missingLibraries = normalized.contains("cannot find -lboost_")
                || normalized.contains("library 'boost_")
                || normalized.contains("library not found for -lboost_");
        return missingHeaders || missingLibraries;
    }

    /** Builds the skip (or, when the build declared Boost mandatory via
     *  -Dcpp.boost.beast.require=true — the sample CI leg where a silently
     *  skipped runtime suite would hide regressions — a failure) for a
     *  missing native dependency. */
    private static RuntimeException unavailableDependency(String what, String detail) {
        if (Boolean.getBoolean("cpp.boost.beast.require")) {
            Assert.fail(what + " but was required for this run: " + detail);
        }
        return new SkipException(what + "; skipping server runtime test: " + detail);
    }
}
