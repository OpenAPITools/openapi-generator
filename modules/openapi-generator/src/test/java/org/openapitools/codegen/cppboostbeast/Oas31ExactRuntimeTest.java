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
        Path includeDirectory = output;
        Path executable = output.resolve("oas31-exact-runtime-test");
        Path validationTemplate = Path.of(
                "src/main/resources/cpp-boost-beast-client/validation-types.mustache");
        String validationHeader = Files.readString(validationTemplate, StandardCharsets.UTF_8)
                .replace("{{>licenseInfo}}", "")
                .replace("{{validateOnDecode}}", "true");
        Files.writeString(output.resolve("ValidationTypes.h"),
                validationHeader, StandardCharsets.UTF_8);
        writeValidationSupportHeaders(
                output,
                "org::openapitools::client::model::detail::schema_validation",
                "ORG_OPENAPITOOLS_CLIENT_MODEL_DETAIL_SCHEMA_VALIDATION");

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
        Assert.assertFalse(
                schemaIrSource.contains("#/components/schemas/Zeta"),
                "raw recovery must not resurrect a component ref removed by normalization");
        String outerUnionHeader = Files.readString(output.resolve("model/OuterUnion.h"));
        Assert.assertTrue(
                outerUnionHeader.contains(
                        "std::variant<std::variant<std::int32_t, std::string>, bool>"),
                "nested compositions must retain an assignable outer branch type");
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

        String apiSource = Files.readString(output.resolve("api/DefaultApi.cpp"));
        int emptyMethodStart = apiSource.indexOf("DefaultApi::getEmpty(");
        int emptyMethodEnd = apiSource.indexOf("DefaultApi::getProbe(", emptyMethodStart);
        Assert.assertTrue(emptyMethodStart >= 0 && emptyMethodEnd > emptyMethodStart,
                "generated API source must contain the empty-response operation");
        String emptyMethod = apiSource.substring(emptyMethodStart, emptyMethodEnd);
        Assert.assertTrue(
                emptyMethod.contains("responseContentType,\n            true,"),
                "a null-default response model must tolerate an empty response body");

        Path source = Path.of(
                "src/test/resources/3_1/cpp-boost-beast-client/"
                        + "oas31-generated-runtime-regression.cpp");
        Path executable = output.resolve("oas31-generated-runtime-regression");
        String compiler = System.getenv().getOrDefault("CXX", "c++");
        compileGeneratedRuntime(compiler, output, source, executable);

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

    @Test
    public void generatedValidationRuntimeDoesNotCollideAcrossClients() throws Exception {
        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(outputRoot, "oas31-multi-client-");
        output.toFile().deleteOnExit();

        String inputSpec = "src/test/resources/3_1/cpp-boost-beast-client/"
                + "schema-ir-lexeme-regression.yaml";
        Path alphaOutput = output.resolve("alpha");
        Path betaOutput = output.resolve("beta");

        CodegenConfigurator alpha = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(inputSpec)
                .setOutputDir(alphaOutput.toString())
                .addAdditionalProperty("modelPackage", "alpha.client.model");
        CodegenConfigurator beta = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(inputSpec)
                .setOutputDir(betaOutput.toString())
                .addAdditionalProperty("modelPackage", "beta.client.model");
        new DefaultGenerator().opts(alpha.toClientOptInput()).generate();
        new DefaultGenerator().opts(beta.toClientOptInput()).generate();

        Path alphaValidator = alphaOutput.resolve("model/Oas31Validator.h");
        Path betaValidator = betaOutput.resolve("model/Oas31Validator.h");
        Assert.assertTrue(Files.exists(alphaValidator), "Alpha validation header must be emitted");
        Assert.assertTrue(Files.exists(betaValidator), "Beta validation header must be emitted");
        String alphaContent = Files.readString(alphaValidator, StandardCharsets.UTF_8);
        String betaContent = Files.readString(betaValidator, StandardCharsets.UTF_8);
        Assert.assertTrue(alphaContent.contains(
                "namespace alpha::client::model::detail::schema_validation {"));
        Assert.assertTrue(betaContent.contains(
                "namespace beta::client::model::detail::schema_validation {"));
        Assert.assertFalse(alphaContent.contains("namespace oas31"));
        Assert.assertFalse(betaContent.contains("namespace oas31"));

        Path source = output.resolve("multi-client-validation.cpp");
        Files.writeString(source, String.join("\n",
                "#include \"alpha/model/Oas31Validator.h\"",
                "#include \"beta/model/Oas31Validator.h\"",
                "",
                "#include <type_traits>",
                "",
                "namespace alpha_validation =",
                "    alpha::client::model::detail::schema_validation;",
                "namespace beta_validation =",
                "    beta::client::model::detail::schema_validation;",
                "",
                "static_assert(!std::is_same_v<",
                "    alpha_validation::ExactNumber,",
                "    beta_validation::ExactNumber>);",
                "",
                "int main() {",
                "    auto const alphaNumber =",
                "        alpha_validation::ExactNumber::parseLexeme(\"1.0\");",
                "    auto const betaNumber =",
                "        beta_validation::ExactNumber::parseLexeme(\"1e0\");",
                "    return (alphaNumber == alpha_validation::ExactNumber::parseLexeme(\"1\")",
                "            && betaNumber == beta_validation::ExactNumber::parseLexeme(\"1\"))",
                "        ? 0 : 1;",
                "}",
                ""), StandardCharsets.UTF_8);

        String compiler = System.getenv().getOrDefault("CXX", "c++");
        List<String> command = List.of(
                compiler,
                "-std=c++17",
                "-Wall",
                "-Wextra",
                "-Werror",
                "-I",
                output.toString(),
                source.toString(),
                "-fsyntax-only");

        Process compile;
        try {
            compile = new ProcessBuilder(command)
                    .redirectErrorStream(true)
                    .start();
        } catch (IOException exception) {
            throw new SkipException(
                    "C++ compiler is unavailable; skipping multi-client namespace test: "
                            + exception.getMessage());
        }

        Assert.assertTrue(compile.waitFor(120, TimeUnit.SECONDS),
                "Multi-client validation compile timed out");
        String compileOutput = new String(
                compile.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        if (compile.exitValue() != 0 && missingBoostHeaders(compileOutput)) {
            throw new SkipException(
                    "Boost headers are unavailable; skipping multi-client namespace test: "
                            + compileOutput.trim());
        }
        Assert.assertEquals(compile.exitValue(), 0,
                "Two generated clients must compile in one translation unit:\n" + compileOutput);
    }

    private static void compileGeneratedRuntime(
            String compiler,
            Path output,
            Path driverSource,
            Path executable) throws Exception {
        List<Path> sources = new ArrayList<>();
        sources.add(driverSource.toAbsolutePath());
        try (java.util.stream.Stream<Path> modelSources =
                     Files.list(output.resolve("model"))) {
            modelSources
                    .filter(path -> path.getFileName().toString().endsWith(".cpp"))
                    .sorted()
                    .map(Path::toAbsolutePath)
                    .forEach(sources::add);
        }

        int workerCount = Math.min(
                4,
                Math.min(Runtime.getRuntime().availableProcessors(), sources.size()));
        List<List<Path>> sourceGroups = new ArrayList<>(workerCount);
        for (int worker = 0; worker < workerCount; worker++) {
            sourceGroups.add(new ArrayList<>());
        }

        Path objectDirectory = Files.createDirectories(output.resolve("native-objects"));
        List<Path> objectFiles = new ArrayList<>(sources.size());
        for (int index = 0; index < sources.size(); index++) {
            Path source = sources.get(index);
            sourceGroups.get(index % workerCount).add(source);
            String sourceName = source.getFileName().toString();
            Assert.assertTrue(sourceName.endsWith(".cpp"),
                    "Native runtime source must have a .cpp suffix: " + source);
            objectFiles.add(objectDirectory.resolve(
                    sourceName.substring(0, sourceName.length() - 4) + ".o").toAbsolutePath());
        }

        List<String> compilePrefix = List.of(
                compiler,
                "-std=c++17",
                "-Wall",
                "-Wextra",
                "-Werror",
                "-I",
                output.toAbsolutePath().toString(),
                "-I",
                output.resolve("api").toAbsolutePath().toString(),
                "-I",
                output.resolve("model").toAbsolutePath().toString());
        List<Process> compilers = new ArrayList<>(workerCount);
        List<Path> compilerLogs = new ArrayList<>(workerCount);
        for (int worker = 0; worker < workerCount; worker++) {
            List<String> command = new ArrayList<>(compilePrefix);
            command.add("-c");
            sourceGroups.get(worker).stream()
                    .map(Path::toString)
                    .forEach(command::add);
            Path compilerLog = objectDirectory.resolve("compile-" + worker + ".log");
            compilerLogs.add(compilerLog);
            try {
                compilers.add(new ProcessBuilder(command)
                        .directory(objectDirectory.toFile())
                        .redirectErrorStream(true)
                        .redirectOutput(compilerLog.toFile())
                        .start());
            } catch (IOException exception) {
                terminateProcesses(compilers);
                throw new SkipException(
                        "C++ compiler is unavailable; skipping generated runtime test: "
                                + exception.getMessage());
            }
        }

        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(240);
        try {
            for (Process process : compilers) {
                long remaining = deadline - System.nanoTime();
                if (remaining <= 0
                        || !process.waitFor(remaining, TimeUnit.NANOSECONDS)) {
                    terminateProcesses(compilers);
                    Assert.fail("Generated C++ runtime object compilation timed out:\n"
                            + readCompilerLogs(compilerLogs));
                }
            }
        } catch (InterruptedException exception) {
            terminateProcesses(compilers);
            throw exception;
        }

        String compileOutput = readCompilerLogs(compilerLogs);
        boolean compileFailed = compilers.stream().anyMatch(process -> process.exitValue() != 0);
        if (compileFailed && missingBoostHeaders(compileOutput)) {
            throw new SkipException(
                    "Boost headers are unavailable; skipping generated runtime test: "
                            + compileOutput.trim());
        }
        Assert.assertFalse(compileFailed,
                "Generated C++ runtime objects must compile cleanly:\n" + compileOutput);

        List<String> linkCommand = new ArrayList<>();
        linkCommand.add(compiler);
        objectFiles.stream().map(Path::toString).forEach(linkCommand::add);
        linkCommand.add("-o");
        linkCommand.add(executable.toAbsolutePath().toString());

        Process linker = new ProcessBuilder(linkCommand)
                .redirectErrorStream(true)
                .start();
        if (!linker.waitFor(120, TimeUnit.SECONDS)) {
            linker.destroyForcibly().waitFor();
            Assert.fail("Generated C++ runtime link timed out");
        }
        String linkOutput = new String(
                linker.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertEquals(linker.exitValue(), 0,
                "Generated C++ runtime must link cleanly:\n" + linkOutput);
    }

    private static String readCompilerLogs(List<Path> compilerLogs) throws IOException {
        StringBuilder output = new StringBuilder();
        for (Path compilerLog : compilerLogs) {
            if (Files.exists(compilerLog)) {
                String contents = Files.readString(compilerLog, StandardCharsets.UTF_8);
                if (!contents.isEmpty()) {
                    output.append(compilerLog.getFileName()).append(":\n")
                            .append(contents);
                }
            }
        }
        return output.toString();
    }

    private static void terminateProcesses(List<Process> processes) {
        for (Process process : processes) {
            process.descendants().forEach(ProcessHandle::destroyForcibly);
            if (process.isAlive()) {
                process.destroyForcibly();
            }
        }
        for (Process process : processes) {
            if (process.isAlive()) {
                try {
                    process.waitFor(10, TimeUnit.SECONDS);
                } catch (InterruptedException exception) {
                    Thread.currentThread().interrupt();
                    return;
                }
            }
        }
    }

    private static void writeValidationSupportHeaders(
            Path output,
            String namespaceName,
            String guardPrefix) throws IOException {
        Path templateDirectory = Path.of("src/main/resources/cpp-boost-beast-client");
        String[][] headers = {
            {"oas31_exact_number.mustache", "Oas31ExactNumber.h"},
            {"oas31_exact_json.mustache", "Oas31ExactJson.h"},
            {"oas31_schema_ir.mustache", "Oas31SchemaIr.h"},
            {"oas31_deep_equal.mustache", "Oas31DeepEqual.h"},
            {"oas31_validator.mustache", "Oas31Validator.h"}
        };
        for (String[] header : headers) {
            String rendered = Files.readString(
                            templateDirectory.resolve(header[0]), StandardCharsets.UTF_8)
                    .replace("{{schemaValidationNamespace}}", namespaceName)
                    .replace("{{schemaValidationHeaderGuardPrefix}}", guardPrefix);
            Files.writeString(output.resolve(header[1]), rendered, StandardCharsets.UTF_8);
        }
    }

    private static boolean missingBoostHeaders(String compilerOutput) {
        String normalized = compilerOutput.toLowerCase(java.util.Locale.ROOT);
        return normalized.contains("boost/")
                && (normalized.contains("not found")
                || normalized.contains("no such file"));
    }
}
