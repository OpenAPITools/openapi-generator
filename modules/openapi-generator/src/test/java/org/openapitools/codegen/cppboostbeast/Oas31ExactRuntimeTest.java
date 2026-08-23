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
