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

package org.openapitools.codegen.cppboostbeast;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class Oas31ExactRuntimeTest {

    @Test
    public void exactRuntimePreservesArbitraryJsonNumbersEndToEnd() throws Exception {
        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(outputRoot, "oas31-exact-runtime-");
        output.toFile().deleteOnExit();

        Path source = Path.of(
                "src/test/resources/3_1/cpp-boost-beast-client/oas31-exact-runtime-test.cpp");
        Path includeDirectory = Path.of("src/main/resources/cpp-boost-beast-client");
        Path executable = output.resolve("oas31-exact-runtime-test");
        Path validationTemplate = Path.of(
                "src/main/resources/cpp-boost-beast-client/validation-types.mustache");
        String validationHeader = Files.readString(validationTemplate, StandardCharsets.UTF_8)
                .replace("{{>licenseInfo}}", "")
                .replace("{{validateOnDecode}}", "true");
        Files.writeString(output.resolve("ValidationTypes.h"),
                validationHeader, StandardCharsets.UTF_8);

        String compiler = System.getenv().getOrDefault("CXX", "c++");
        List<String> command = new ArrayList<>();
        command.add(compiler);
        command.add("-std=c++17");
        command.add("-Wall");
        command.add("-Wextra");
        command.add("-Werror");
        command.add("-I");
        command.add(output.toString());
        command.add("-I");
        command.add(includeDirectory.toString());
        command.add(source.toString());
        command.add("-o");
        command.add(executable.toString());

        Process compile;
        try {
            compile = new ProcessBuilder(command)
                    .redirectErrorStream(true)
                    .start();
        } catch (IOException exception) {
            throw new SkipException(
                    "C++ compiler is unavailable; skipping native runtime test: "
                            + exception.getMessage());
        }

        Assert.assertTrue(compile.waitFor(120, TimeUnit.SECONDS),
                "C++ exact-runtime test compilation timed out");
        String compileOutput = new String(
                compile.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        if (compile.exitValue() != 0 && missingBoostHeaders(compileOutput)) {
            throw new SkipException(
                    "Boost headers are unavailable; skipping native runtime test: "
                            + compileOutput.trim());
        }
        Assert.assertEquals(compile.exitValue(), 0,
                "C++ exact-runtime test must compile cleanly:\n" + compileOutput);

        Process run = new ProcessBuilder(executable.toString())
                .redirectErrorStream(true)
                .start();
        Assert.assertTrue(run.waitFor(30, TimeUnit.SECONDS),
                "C++ exact-runtime test execution timed out");
        String runOutput = new String(
                run.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertEquals(run.exitValue(), 0,
                "C++ exact-runtime test failed:\n" + runOutput);
        Assert.assertTrue(runOutput.contains("oas31 exact runtime tests passed"),
                "C++ exact-runtime test did not report completion: " + runOutput);
    }

    @Test
    public void generatedClientHonorsCompositionAndParameterWireSemantics() throws Exception {
        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(outputRoot, "oas31-generated-runtime-");
        output.toFile().deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(
                        "src/test/resources/3_1/cpp-boost-beast-client/oas31-runtime-regression.yaml")
                .setOutputDir(output.toString())
                .addAdditionalProperty("validateOnDecode", true);
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        String mixedModelSource = Files.readString(output.resolve("model/Mixed.cpp"));
        Assert.assertTrue(
                mixedModelSource.contains("schemaNodeFor(\"mixed_component\")"),
                "decode validation must use the raw component schema id");
        String schemaIrSource = Files.readString(
                output.resolve("model/schema_ir.generated.cpp"));
        Assert.assertTrue(
                schemaIrSource.contains("allNull_branch_0")
                        && schemaIrSource.contains("allNull_branch_1")
                        && schemaIrSource.contains("duplicateNull_branch_0")
                        && schemaIrSource.contains("duplicateNull_branch_1"),
                "raw duplicate-null branches must each receive a schema IR row");
        int patternNodeEnd = schemaIrSource.indexOf(
                "n.sourceName = \"nonAsciiPattern_branch_0\"");
        Assert.assertTrue(patternNodeEnd >= 0,
                "non-ASCII pattern schema must receive a schema IR row");
        int patternNodeStart = schemaIrSource.lastIndexOf("\n    { // node", patternNodeEnd);
        Assert.assertTrue(patternNodeStart >= 0,
                "non-ASCII pattern schema row must have a generated node block");
        String patternNode = schemaIrSource.substring(patternNodeStart, patternNodeEnd);
        Assert.assertTrue(
                patternNode.contains("AdditionalPropertiesKind::reject")
                        && patternNode.contains("patternProperties.push_back"),
                "raw patternProperties and additionalProperties must survive inline-model normalization");

        Path source = Path.of(
                "src/test/resources/3_1/cpp-boost-beast-client/"
                        + "oas31-generated-runtime-regression.cpp");
        Path executable = output.resolve("oas31-generated-runtime-regression");
        String compiler = System.getenv().getOrDefault("CXX", "c++");
        List<String> command = new ArrayList<>();
        command.add(compiler);
        command.add("-std=c++17");
        command.add("-Wall");
        command.add("-Wextra");
        command.add("-Werror");
        command.add("-I");
        command.add(output.toString());
        command.add("-I");
        command.add(output.resolve("api").toString());
        command.add("-I");
        command.add(output.resolve("model").toString());
        command.add(source.toString());
        try (java.util.stream.Stream<Path> modelSources =
                     Files.list(output.resolve("model"))) {
            modelSources
                    .filter(path -> path.getFileName().toString().endsWith(".cpp"))
                    .sorted()
                    .map(Path::toString)
                    .forEach(command::add);
        }
        command.add("-o");
        command.add(executable.toString());

        Process compile;
        try {
            compile = new ProcessBuilder(command)
                    .redirectErrorStream(true)
                    .start();
        } catch (IOException exception) {
            throw new SkipException(
                    "C++ compiler is unavailable; skipping generated runtime test: "
                            + exception.getMessage());
        }

        Assert.assertTrue(compile.waitFor(180, TimeUnit.SECONDS),
                "Generated C++ runtime test compilation timed out");
        String compileOutput = new String(
                compile.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        if (compile.exitValue() != 0 && missingBoostHeaders(compileOutput)) {
            throw new SkipException(
                    "Boost headers are unavailable; skipping generated runtime test: "
                            + compileOutput.trim());
        }
        Assert.assertEquals(compile.exitValue(), 0,
                "Generated C++ runtime test must compile cleanly:\n" + compileOutput);

        Process run = new ProcessBuilder(executable.toString())
                .redirectErrorStream(true)
                .start();
        Assert.assertTrue(run.waitFor(30, TimeUnit.SECONDS),
                "Generated C++ runtime test execution timed out");
        String runOutput = new String(
                run.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertEquals(run.exitValue(), 0,
                "Generated C++ runtime test failed:\n" + runOutput);
        Assert.assertTrue(
                runOutput.contains("oas31 generated runtime regressions passed"),
                "Generated C++ runtime test did not report completion: " + runOutput);
    }

    private static boolean missingBoostHeaders(String compilerOutput) {
        String normalized = compilerOutput.toLowerCase(java.util.Locale.ROOT);
        return normalized.contains("boost/")
                && (normalized.contains("not found")
                || normalized.contains("no such file"));
    }
}
