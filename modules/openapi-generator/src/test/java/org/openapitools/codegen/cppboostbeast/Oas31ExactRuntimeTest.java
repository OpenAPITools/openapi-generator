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

import com.samskivert.mustache.Mustache;
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
        String modelNamespace = "org::openapitools::client::model";
        String validationNamespace = modelNamespace + "::detail::schema_validation";
        String validationGuard =
                "ORG_OPENAPITOOLS_CLIENT_MODEL_VALIDATION_TYPES";
        String validationHeader = Files.readString(validationTemplate, StandardCharsets.UTF_8)
                .replace("{{>licenseInfo}}", "")
                .replace("{{validateOnDecode}}", "true")
                .replace("{{schemaValidationHeaderGuardPrefix}}", validationGuard)
                .replace("{{#modelNamespaceDeclarations}}\nnamespace {{this}} {\n"
                        + "{{/modelNamespaceDeclarations}}",
                        "namespace " + modelNamespace + " {")
                .replace("{{#modelNamespaceDeclarations}}\n}\n"
                        + "{{/modelNamespaceDeclarations}}", "}");
        Files.writeString(output.resolve("ValidationTypes.h"),
                validationHeader, StandardCharsets.UTF_8);
        writeValidationSupportHeaders(output, validationNamespace,
                validationGuard + "_SCHEMA_VALIDATION");

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
        command.add(output.resolve("Oas31ExactNumber.cpp").toString());
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
                .addAdditionalProperty("validateOnDecode", true)
                .addAdditionalProperty("sseSchemaMode", "jsonEventData");
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        String mixedModelSource = Files.readString(output.resolve("model/Mixed.cpp"));
        Assert.assertTrue(
                mixedModelSource.contains("schemaNodeFor(\"mixed_component\")"),
                "decode validation must use the raw component schema id");
        String scalarDefaultsSource = Files.readString(
                output.resolve("model/ScalarDefaults.cpp"));
        String scalarDefaultsHeader = Files.readString(
                output.resolve("model/ScalarDefaults.h"));
        Assert.assertTrue(scalarDefaultsSource.contains(
                        "NullableField<std::string>::makeDefaultNull()")
                        && scalarDefaultsSource.contains(
                        "NullableField<std::string>::makeDefaultValue(\"fallback\")")
                        && scalarDefaultsHeader.contains(
                        "m_Nullable_label = NullableField<std::string>::makeDefaultNull()")
                        && scalarDefaultsHeader.contains(
                        "m_Nullable_fallback = NullableField<std::string>::makeDefaultValue(\"fallback\")"),
                "nullable schema defaults must initialize constructors and decode resets");
        Assert.assertTrue(
                Files.exists(output.resolve("model/ModelBranchNullDefault.h")),
                "a null default on a model-valued anyOf branch must not abort generation");
        String composedDefaultSource = Files.readString(
                output.resolve("model/ComposedDefaultContainer.cpp"));
        String composedDefaultHeader = Files.readString(
                output.resolve("model/ComposedDefaultContainer.h"));
        String composedDefaultExpression =
                "fromJsonValue_DefaultVoice(boost::json::value(\"alloy\"))";
        Assert.assertTrue(
                composedDefaultSource.contains("m_Voice = " + composedDefaultExpression)
                        && composedDefaultHeader.contains(
                        "DefaultVoice m_Voice = " + composedDefaultExpression),
                "composed schema defaults must decode into the generated variant type");
        String nullableEnumHeader = Files.readString(
                output.resolve("model/NullableEnumBox.h"));
        Assert.assertTrue(nullableEnumHeader.contains(
                        "std::optional<std::string> m_Required_enum;"),
                "implicit primitive placeholders must not initialize nullable fields");
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
                        "std::variant<std::variant<CompositionBranchValue<0, std::string>, "
                                + "CompositionBranchValue<1, std::string>>, bool>"),
                "nested tagged compositions must retain an assignable outer branch type");
        String outerUnionSource = Files.readString(output.resolve("model/OuterUnion.cpp"));
        Assert.assertFalse(
                outerUnionSource.contains("OuterUnion(CompositionBranchValue<"),
                "nested tags must not mark distinct outer alternatives as tagged");
        String taggedContainerSource = Files.readString(
                output.resolve("model/TaggedUnionContainer.cpp"));
        Assert.assertTrue(
                taggedContainerSource.contains(
                        "struct JsonValueConverter<CompositionBranchValue<BranchIndex, ValueType>>"),
                "property conversion must unwrap tagged composition alternatives");
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
        Assert.assertTrue(
                apiSource.contains("setStream(NullableField<bool>{false})")
                        && apiSource.contains("setStream(NullableField<bool>{true})"),
                "conditional SSE selectors must construct their nullable setter type explicitly");
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

        Path sseTransportSource = Path.of(
                "src/test/resources/3_1/cpp-boost-beast-client/"
                        + "oas31-generated-sse-transport-regression.cpp");
        Path sseTransportExecutable = output.resolve(
                "oas31-generated-sse-transport-regression");
        compileGeneratedSseTransport(
                compiler, output, sseTransportSource, sseTransportExecutable);

        Process sseTransportRun = new ProcessBuilder(sseTransportExecutable.toString())
                .redirectErrorStream(true)
                .start();
        Assert.assertTrue(sseTransportRun.waitFor(30, TimeUnit.SECONDS),
                "Generated SSE transport test execution timed out");
        String sseTransportOutput = new String(
                sseTransportRun.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertEquals(sseTransportRun.exitValue(), 0,
                "Generated SSE transport test failed:\n" + sseTransportOutput);
        Assert.assertTrue(
                sseTransportOutput.contains("oas31 SSE transport regressions passed"),
                "Generated SSE transport test did not report completion: "
                        + sseTransportOutput);
    }

    @Test
    public void generatedClientPreservesAdditionalPropertiesWhenEnabled() throws Exception {
        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(
                outputRoot, "cpp-boost-beast-preserve-additional-properties-runtime-");
        output.toFile().deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/"
                        + "preserve-additional-properties.yaml")
                .setOutputDir(output.toString())
                .addAdditionalProperty("validateOnDecode", true)
                .addAdditionalProperty("preserveAdditionalProperties", true);
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        Path source = Path.of("src/test/resources/3_1/cpp-boost-beast-client/"
                + "preserve-additional-properties-runtime-regression.cpp");
        Path executable = output.resolve("preserve-additional-properties-runtime-regression");
        String compiler = System.getenv().getOrDefault("CXX", "c++");
        compileGeneratedRuntime(compiler, output, source, executable);

        Process run = new ProcessBuilder(executable.toString())
                .redirectErrorStream(true)
                .start();
        Assert.assertTrue(run.waitFor(30, TimeUnit.SECONDS),
                "Additional-property preservation runtime execution timed out");
        String runOutput = new String(
                run.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertEquals(run.exitValue(), 0,
                "Additional-property preservation runtime failed:\n" + runOutput);
        Assert.assertTrue(
                runOutput.contains("preserve additional properties runtime regressions passed"),
                "Additional-property preservation runtime did not report completion: "
                        + runOutput);
    }

    @Test
    public void compileWithValidationFalseStripsSchemaValidationAndKeepsRepresentationChecks()
            throws Exception {
        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(outputRoot, "oas31-validation-disabled-");
        output.toFile().deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(
                        "src/test/resources/3_1/cpp-boost-beast-client/oas31-runtime-regression.yaml")
                .setOutputDir(output.toString())
                .addAdditionalProperty("compileWithValidation", false);
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        Path modelDirectory = output.resolve("model");
        for (String omittedFile : List.of(
                "Oas31SchemaRegistry.h",
                "schema_ir.generated.cpp")) {
            Assert.assertFalse(Files.exists(modelDirectory.resolve(omittedFile)),
                    omittedFile + " must not be emitted when validation is disabled");
        }
        try (java.util.stream.Stream<Path> files = Files.list(modelDirectory)) {
            Assert.assertFalse(files.anyMatch(path -> path.getFileName().toString()
                            .startsWith("schema_ir.generated.chunk")),
                    "Schema IR chunk translation units must not be emitted when validation is disabled");
        }

        for (String retainedHeader : List.of(
                "Oas31ExactNumber.h",
                "Oas31SchemaIr.h",
                "Oas31Validator.h")) {
            Assert.assertTrue(Files.exists(modelDirectory.resolve(retainedHeader)),
                    retainedHeader + " must remain available as a header-only utility");
        }
        String validationTypes = Files.readString(modelDirectory.resolve("ValidationTypes.h"));
        Assert.assertTrue(validationTypes.contains("kValidateOnDecode = false"),
                "ValidationTypes.h must expose the disabled compile-time mode");

        String cmakeLists = Files.readString(output.resolve("CMakeLists.txt"));
        Assert.assertFalse(cmakeLists.contains("schema_ir.generated"),
                "Generated CMake must not reference stripped schema IR sources");
        Assert.assertFalse(cmakeLists.contains("Oas31SchemaRegistry.h"),
                "Generated CMake must not reference the stripped schema registry");

        List<Path> modelSources;
        try (java.util.stream.Stream<Path> sources = Files.list(modelDirectory)) {
            modelSources = sources
                    .filter(path -> path.getFileName().toString().endsWith(".cpp"))
                    .collect(java.util.stream.Collectors.toList());
        }
        Assert.assertFalse(modelSources.isEmpty(), "Disabled client must still emit model sources");
        for (Path modelSource : modelSources) {
            String contents = Files.readString(modelSource);
            Assert.assertFalse(contents.contains("bool validate_"),
                    modelSource.getFileName() + " must not define per-branch validators");
            Assert.assertFalse(contents.contains("#include \"Oas31SchemaRegistry.h\""),
                    modelSource.getFileName() + " must not include the stripped schema registry");
            Assert.assertFalse(contents.contains("#include \"Oas31Validator.h\""),
                    modelSource.getFileName() + " must not include the schema evaluator");
        }

        Path driverSource = Path.of(
                "src/test/resources/3_1/cpp-boost-beast-client/"
                        + "oas31-validation-disabled-runtime.cpp");
        Path executable = output.resolve("oas31-validation-disabled-runtime");
        String compiler = System.getenv().getOrDefault("CXX", "c++");
        compileGeneratedRuntime(compiler, output, driverSource, executable);

        Process run = new ProcessBuilder(executable.toString())
                .redirectErrorStream(true)
                .start();
        Assert.assertTrue(run.waitFor(30, TimeUnit.SECONDS),
                "Validation-disabled runtime test execution timed out");
        String runOutput = new String(
                run.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertEquals(run.exitValue(), 0,
                "Validation-disabled runtime test failed:\n" + runOutput);
        Assert.assertTrue(runOutput.contains("validation-disabled runtime checks passed"),
                "Validation-disabled runtime test did not report completion: " + runOutput);
    }

    @Test
    public void generatedValidationRuntimeDoesNotCollideAcrossClients() throws Exception {
        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(outputRoot, "oas31-multi-client-");
        output.toFile().deleteOnExit();

        String inputSpec = "src/test/resources/3_1/cpp-boost-beast-client/"
                + "oas31-runtime-regression.yaml";
        Path alphaOutput = output.resolve("alpha");
        Path betaOutput = output.resolve("beta");

        CodegenConfigurator alpha = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(inputSpec)
                .setOutputDir(alphaOutput.toString())
                .addAdditionalProperty("packageName", "AlphaClient")
                .addAdditionalProperty("modelPackage", "alpha.client.model")
                .addAdditionalProperty("apiPackage", "alpha.client.api")
                .addAdditionalProperty("compileWithValidation", true);
        CodegenConfigurator beta = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(inputSpec)
                .setOutputDir(betaOutput.toString())
                .addAdditionalProperty("packageName", "BetaClient")
                .addAdditionalProperty("modelPackage", "beta.client.model")
                .addAdditionalProperty("apiPackage", "beta.client.api")
                .addAdditionalProperty("compileWithValidation", false);
        new DefaultGenerator().opts(alpha.toClientOptInput()).generate();
        new DefaultGenerator().opts(beta.toClientOptInput()).generate();

        String alphaValidationTypes = Files.readString(
                alphaOutput.resolve("model/ValidationTypes.h"), StandardCharsets.UTF_8);
        String betaValidationTypes = Files.readString(
                betaOutput.resolve("model/ValidationTypes.h"), StandardCharsets.UTF_8);
        Assert.assertTrue(alphaValidationTypes.contains("kValidateOnDecode = true"));
        Assert.assertTrue(betaValidationTypes.contains("kValidateOnDecode = false"));

        Path source = output.resolve("multi-client-link.cpp");
        Files.writeString(source, String.join("\n",
                "#include <boost/json.hpp>",
                "#include <memory>",
                "#include <string>",
                "#include <type_traits>",
                "",
                "#include \"alpha/model/AnyType.h\"",
                "#include \"alpha/model/NullableField.h\"",
                "#include \"alpha/model/ValidationTypes.h\"",
                "#include \"alpha/model/Oas31Validator.h\"",
                "#include \"alpha/model/ScalarDefaults.h\"",
                "#include \"alpha/api/DefaultApi.h\"",
                "#include \"beta/model/AnyType.h\"",
                "#include \"beta/model/NullableField.h\"",
                "#include \"beta/model/ValidationTypes.h\"",
                "#include \"beta/model/Oas31Validator.h\"",
                "#include \"beta/model/ScalarDefaults.h\"",
                "#include \"beta/api/DefaultApi.h\"",
                "",
                "namespace alpha_model = alpha::client::model;",
                "namespace beta_model = beta::client::model;",
                "namespace alpha_api = alpha::client::api;",
                "namespace beta_api = beta::client::api;",
                "namespace alpha_validation = alpha_model::detail::schema_validation;",
                "namespace beta_validation = beta_model::detail::schema_validation;",
                "",
                "static_assert(!std::is_same_v<",
                "    alpha_model::NullableField<std::string>,",
                "    beta_model::NullableField<std::string>>);",
                "static_assert(!std::is_same_v<",
                "    alpha_model::ValidationResult, beta_model::ValidationResult>);",
                "static_assert(!std::is_same_v<",
                "    alpha_validation::ExactNumber, beta_validation::ExactNumber>);",
                "static_assert(!std::is_same_v<alpha_api::HttpClient, beta_api::HttpClient>);",
                "",
                "int main() {",
                "    alpha_model::ScalarDefaults alphaDefaults;",
                "    beta_model::ScalarDefaults betaDefaults;",
                "    alpha_api::DefaultApi alphaClient{std::shared_ptr<alpha_api::HttpClient>()};",
                "    beta_api::DefaultApi betaClient{std::shared_ptr<beta_api::HttpClient>()};",
                "    (void)alphaClient;",
                "    (void)betaClient;",
                "    return (!alphaDefaults.isEnabled() && !betaDefaults.isEnabled()",
                "            && alphaDefaults.getRetries() == -7",
                "            && betaDefaults.getRetries() == -7) ? 0 : 1;",
                "}",
                ""), StandardCharsets.UTF_8);

        String compiler = System.getenv().getOrDefault("CXX", "c++");
        Path executable = output.resolve("multi-client-link");
        compileAndLinkGeneratedClients(
                compiler, List.of(alphaOutput, betaOutput), source, executable);

        Process run = new ProcessBuilder(executable.toString())
                .redirectErrorStream(true)
                .start();
        Assert.assertTrue(run.waitFor(30, TimeUnit.SECONDS),
                "Multi-client executable timed out");
        String runOutput = new String(
                run.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertEquals(run.exitValue(), 0,
                "Two generated clients must run in one executable:\n" + runOutput);
    }

    private static void compileAndLinkGeneratedClients(
            String compiler,
            List<Path> outputs,
            Path driverSource,
            Path executable) throws Exception {
        List<Path> sources = new ArrayList<>();
        sources.add(driverSource.toAbsolutePath());
        for (Path output : outputs) {
            try (java.util.stream.Stream<Path> modelSources =
                         Files.list(output.resolve("model"));
                 java.util.stream.Stream<Path> apiSources =
                         Files.list(output.resolve("api"))) {
                modelSources
                        .filter(path -> path.getFileName().toString().endsWith(".cpp"))
                        .sorted()
                        .map(Path::toAbsolutePath)
                        .forEach(sources::add);
                apiSources
                        .filter(path -> path.getFileName().toString().endsWith(".cpp"))
                        .sorted()
                        .map(Path::toAbsolutePath)
                        .forEach(sources::add);
            }
        }

        List<String> compilePrefix = new ArrayList<>(List.of(
                compiler,
                "-std=c++17",
                "-Wall",
                "-Wextra",
                "-Werror",
                "-DBOOST_ERROR_CODE_HEADER_ONLY",
                "-pthread"));
        for (Path output : outputs) {
            compilePrefix.add("-I");
            compilePrefix.add(output.toAbsolutePath().toString());
            compilePrefix.add("-I");
            compilePrefix.add(output.resolve("api").toAbsolutePath().toString());
            compilePrefix.add("-I");
            compilePrefix.add(output.resolve("model").toAbsolutePath().toString());
        }

        Path objectDirectory = Files.createDirectories(
                executable.getParent().resolve("multi-client-objects"));
        List<Path> objectFiles = new ArrayList<>(sources.size());
        List<Path> compilerLogs = new ArrayList<>(sources.size());
        List<List<String>> compileCommands = new ArrayList<>(sources.size());
        for (int index = 0; index < sources.size(); index++) {
            Path source = sources.get(index);
            String sourceName = source.getFileName().toString();
            Assert.assertTrue(sourceName.endsWith(".cpp"),
                    "Multi-client source must have a .cpp suffix: " + source);
            Path objectFile = objectDirectory.resolve(
                    index + "-" + sourceName.substring(0, sourceName.length() - 4)
                            + ".o").toAbsolutePath();
            objectFiles.add(objectFile);
            compilerLogs.add(objectDirectory.resolve("compile-" + index + ".log"));

            List<String> command = new ArrayList<>(compilePrefix);
            command.add("-c");
            command.add(source.toString());
            command.add("-o");
            command.add(objectFile.toString());
            compileCommands.add(command);
        }

        boolean compiled = compileTranslationUnitsInParallel(
                compileCommands, compilerLogs, 240);
        String compileOutput = readCompilerLogs(compilerLogs);
        if (!compiled && missingSseTransportDependencies(compileOutput)) {
            throw new SkipException(
                    "Boost/OpenSSL development files are unavailable; "
                            + "skipping multi-client link test: "
                            + compileOutput.trim());
        }
        Assert.assertTrue(compiled,
                "Two generated clients must compile cleanly:\n" + compileOutput);

        List<String> linkCommand = new ArrayList<>();
        linkCommand.add(compiler);
        objectFiles.stream().map(Path::toString).forEach(linkCommand::add);
        linkCommand.add("-o");
        linkCommand.add(executable.toAbsolutePath().toString());
        linkCommand.add("-pthread");
        linkCommand.add("-lboost_json");
        linkCommand.add("-lssl");
        linkCommand.add("-lcrypto");

        Process linker = new ProcessBuilder(linkCommand)
                .redirectErrorStream(true)
                .start();
        if (!linker.waitFor(120, TimeUnit.SECONDS)) {
            terminateProcesses(List.of(linker));
            Assert.fail("Multi-client link timed out");
        }
        String linkOutput = new String(
                linker.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        if (linker.exitValue() != 0
                && missingSseTransportDependencies(linkOutput)) {
            throw new SkipException(
                    "Boost/OpenSSL development files are unavailable; "
                            + "skipping multi-client link test: "
                            + linkOutput.trim());
        }
        Assert.assertEquals(linker.exitValue(), 0,
                "Two generated clients must link cleanly:\n" + linkOutput);
    }

    private static boolean compileTranslationUnitsInParallel(
            List<List<String>> commands,
            List<Path> compilerLogs,
            long timeoutSeconds) throws Exception {
        int workerCount = Math.min(
                4,
                Math.min(Runtime.getRuntime().availableProcessors(), commands.size()));
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(timeoutSeconds);

        for (int first = 0; first < commands.size(); first += workerCount) {
            int last = Math.min(first + workerCount, commands.size());
            List<Process> compilers = new ArrayList<>(last - first);
            try {
                for (int index = first; index < last; index++) {
                    compilers.add(new ProcessBuilder(commands.get(index))
                            .redirectErrorStream(true)
                            .redirectOutput(compilerLogs.get(index).toFile())
                            .start());
                }
            } catch (IOException exception) {
                terminateProcesses(compilers);
                throw new SkipException(
                        "C++ compiler is unavailable; skipping generated C++ compilation: "
                                + exception.getMessage());
            }

            try {
                for (Process compilerProcess : compilers) {
                    long remaining = deadline - System.nanoTime();
                    if (remaining <= 0
                            || !compilerProcess.waitFor(
                                    remaining, TimeUnit.NANOSECONDS)) {
                        terminateProcesses(compilers);
                        Assert.fail("Generated C++ compilation timed out:\n"
                                + readCompilerLogs(compilerLogs));
                    }
                }
            } catch (InterruptedException exception) {
                terminateProcesses(compilers);
                throw exception;
            }

            if (compilers.stream().anyMatch(process -> process.exitValue() != 0)) {
                return false;
            }
        }
        return true;
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

        Path objectDirectory = Files.createDirectories(output.resolve("native-objects"));
        List<Path> objectFiles = new ArrayList<>(sources.size());
        List<Path> compilerLogs = new ArrayList<>(sources.size());
        List<List<String>> compileCommands = new ArrayList<>(sources.size());
        for (int index = 0; index < sources.size(); index++) {
            Path source = sources.get(index);
            String sourceName = source.getFileName().toString();
            Assert.assertTrue(sourceName.endsWith(".cpp"),
                    "Native runtime source must have a .cpp suffix: " + source);
            Path objectFile = objectDirectory.resolve(
                    index + "-" + sourceName.substring(0, sourceName.length() - 4)
                            + ".o").toAbsolutePath();
            objectFiles.add(objectFile);
            compilerLogs.add(objectDirectory.resolve("compile-" + index + ".log"));

            List<String> command = new ArrayList<>(compilePrefix);
            command.add("-c");
            command.add(source.toString());
            command.add("-o");
            command.add(objectFile.toString());
            compileCommands.add(command);
        }

        boolean compiled = compileTranslationUnitsInParallel(
                compileCommands, compilerLogs, 240);
        String compileOutput = readCompilerLogs(compilerLogs);
        if (!compiled && missingBoostHeaders(compileOutput)) {
            throw new SkipException(
                    "Boost headers are unavailable; skipping generated runtime test: "
                            + compileOutput.trim());
        }
        Assert.assertTrue(compiled,
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

    private static void compileGeneratedSseTransport(
            String compiler,
            Path output,
            Path driverSource,
            Path executable) throws Exception {
        List<String> command = new ArrayList<>(List.of(
                compiler,
                "-std=c++17",
                "-Wall",
                "-Wextra",
                "-Werror",
                "-DBOOST_ERROR_CODE_HEADER_ONLY",
                "-I",
                output.toAbsolutePath().toString(),
                driverSource.toAbsolutePath().toString(),
                "-o",
                executable.toAbsolutePath().toString(),
                "-pthread",
                "-lboost_json",
                "-lssl",
                "-lcrypto"));

        Process compilerProcess;
        try {
            compilerProcess = new ProcessBuilder(command)
                    .redirectErrorStream(true)
                    .start();
        } catch (IOException exception) {
            throw new SkipException(
                    "C++ compiler is unavailable; skipping SSE transport test: "
                            + exception.getMessage());
        }
        Assert.assertTrue(compilerProcess.waitFor(180, TimeUnit.SECONDS),
                "Generated SSE transport compilation timed out");
        String compilerOutput = new String(
                compilerProcess.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        if (compilerProcess.exitValue() != 0
                && missingSseTransportDependencies(compilerOutput)) {
            throw new SkipException(
                    "Boost/OpenSSL development files are unavailable; "
                            + "skipping SSE transport test: " + compilerOutput.trim());
        }
        Assert.assertEquals(compilerProcess.exitValue(), 0,
                "Generated SSE transport must compile and link cleanly:\n"
                        + compilerOutput);
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
            String template = Files.readString(
                    templateDirectory.resolve(header[0]), StandardCharsets.UTF_8);
            String rendered = Mustache.compiler().compile(template).execute(
                    java.util.Map.of(
                            "schemaValidationNamespace", namespaceName,
                            "schemaValidationHeaderGuardPrefix", guardPrefix,
                            "hasExportMacro", false,
                            "exportMacro", ""));
            Files.writeString(output.resolve(header[1]), rendered, StandardCharsets.UTF_8);
        }
        String exactNumberSource = Files.readString(
                        templateDirectory.resolve("oas31_exact_number_source.mustache"),
                        StandardCharsets.UTF_8)
                .replace("{{schemaValidationNamespace}}", namespaceName);
        Files.writeString(output.resolve("Oas31ExactNumber.cpp"),
                exactNumberSource, StandardCharsets.UTF_8);
    }

    private static boolean missingSseTransportDependencies(String compilerOutput) {
        String normalized = compilerOutput.toLowerCase(java.util.Locale.ROOT);
        boolean missingOpenSslHeaders = normalized.contains("openssl/")
                && (normalized.contains("not found")
                || normalized.contains("no such file"));
        boolean missingBoostJsonLibrary =
                normalized.contains("cannot find -lboost_json")
                || normalized.contains("library 'boost_json' not found")
                || normalized.contains("library not found for -lboost_json");
        boolean missingOpenSslLibraries =
                normalized.contains("cannot find -lssl")
                || normalized.contains("cannot find -lcrypto")
                || normalized.contains("library 'ssl' not found")
                || normalized.contains("library 'crypto' not found")
                || normalized.contains("library not found for -lssl")
                || normalized.contains("library not found for -lcrypto");
        return missingBoostHeaders(compilerOutput)
                || missingBoostJsonLibrary
                || missingOpenSslHeaders
                || missingOpenSslLibraries;
    }

    private static boolean missingBoostHeaders(String compilerOutput) {
        String normalized = compilerOutput.toLowerCase(java.util.Locale.ROOT);
        return normalized.contains("boost/")
                && (normalized.contains("not found")
                || normalized.contains("no such file"));
    }
}
