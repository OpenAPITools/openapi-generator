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
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Generates a server from the OAS 3.1 regression spec, compiles it together
 * with the loopback driver, runs it against real sockets, and asserts the
 * sentinel output. This is the end-to-end behavior proof for the generator:
 * routing, parameter codecs, body decoding, security, and error mapping all
 * execute in the produced C++ binary.
 */
public class CppBoostBeastServerRuntimeTest {

    @Test
    public void generatedServerServesLoopbackRegressions() throws Exception {
        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(
                outputRoot, "cpp-boost-beast-server-runtime-");
        output.toFile().deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-server")
                .setInputSpec(
                        "src/test/resources/3_1/cpp-boost-beast-server/server-regression.yaml")
                .setOutputDir(output.toString());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        Path driver = Path.of(
                "src/test/resources/3_1/cpp-boost-beast-server/"
                        + "server-runtime-regression.cpp");
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
        command.add(driver.toString());
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

        Process compile = new ProcessBuilder(command)
                .redirectErrorStream(true)
                .directory(new File("."))
                .start();
        Assert.assertTrue(compile.waitFor(10, TimeUnit.MINUTES),
                "server runtime compile timed out");
        String compileOutput = new String(
                compile.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        if (compile.exitValue() != 0 && missingBoost(compileOutput)) {
            throw new org.testng.SkipException(
                    "Boost development files are unavailable; "
                            + "skipping server runtime test: "
                            + compileOutput.trim());
        }
        Assert.assertEquals(compile.exitValue(), 0,
                "server runtime compile failed:\n" + compileOutput);

        Process run = new ProcessBuilder(executable.toString())
                .redirectErrorStream(true)
                .start();
        Assert.assertTrue(run.waitFor(120, TimeUnit.SECONDS),
                "server runtime execution timed out");
        String runOutput = new String(
                run.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertEquals(run.exitValue(), 0,
                "server runtime test failed:\n" + runOutput);
        Assert.assertTrue(
                runOutput.contains("cpp-boost-beast-server runtime regressions passed"),
                "server runtime test did not report completion: " + runOutput);
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
}
