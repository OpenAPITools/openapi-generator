/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.cppboostbeast;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.OpenAPINormalizer;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.CppBoostBeastClientCodegen;
import org.openapitools.codegen.languages.Oas31CompositionLowering;
import org.openapitools.codegen.languages.Oas31KeywordScanner;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CompositionDescriptorTest extends CppBoostBeastNormalizerTestSupport {
    @Test
    public void oneOfStringStringEnumViaGateFixtures() throws IOException {
        // oneOf open-string + string-enum preserves identity via CompositionBranchValue.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-oneof").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/fixtures/composition-fixtures.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastOneOfTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path oneOfHeader = output.toPath().resolve("model/OneOfStringStringEnum.h");
        TestUtils.assertFileExists(oneOfHeader);
        String oneOfContent = java.nio.file.Files.readString(oneOfHeader);
        Assert.assertTrue(oneOfContent.contains("CompositionBranchValue<0, std::string>"),
                "OneOfStringStringEnum should use CompositionBranchValue to preserve branch identity");
        Assert.assertFalse(oneOfContent.contains("using OneOfStringStringEnum = std::string;"),
                "OneOfStringStringEnum must not blind-collapse to std::string");
        Assert.assertFalse(oneOfContent.contains("using OneOfStringStringEnum = boost::json::value;"),
                "OneOfStringStringEnum must not type-erase to boost::json::value");
    }

    @Test
    public void allNullAnyOfViaGateFixtures() throws IOException {
        // Verify that AllNullAnyOf (anyOf [null, null]) in the compliance fixtures
        // produces CompositionBranchValue variant (not boost::json::value).
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-allnull").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/fixtures/composition-fixtures.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastAllNullTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path allNullHeader = output.toPath().resolve("model/AllNullAnyOf.h");
        TestUtils.assertFileExists(allNullHeader);
        String allNullContent = java.nio.file.Files.readString(allNullHeader);
        Assert.assertTrue(allNullContent.contains("CompositionBranchValue<0, std::nullptr_t>"),
                "AllNullAnyOf should use CompositionBranchValue to preserve null identity");
    }

    @Test
    public void duplicateNullOneOfViaGateFixtures() throws IOException {
        // Verify that DuplicateNullOneOf (oneOf [null, null]) in the compliance fixtures
        // generates without error.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-dupenull").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/fixtures/composition-fixtures.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastDupNullTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path dupNullHeader = output.toPath().resolve("model/DuplicateNullOneOf.h");
        TestUtils.assertFileExists(dupNullHeader);
        String dupNullContent = java.nio.file.Files.readString(dupNullHeader);
        Assert.assertTrue(dupNullContent.contains("DuplicateNullOneOf"),
                "DuplicateNullOneOf header must contain the type name");
    }

    @Test
    public void buildsCompositionDescriptorsInPreprocessOpenAPI() {
        // Composition descriptors must be built in preprocessOpenAPI (after
        // normalization and inline flattening) so they exist before any
        // fromModel call. If the generator pipeline ordering changes, this
        // test will catch it.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // Create an OpenAPI with oneOf, anyOf, and allOf schemas
        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        openAPI.setServers(new java.util.ArrayList<>());
        io.swagger.v3.oas.models.Components components =
                new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new java.util.LinkedHashMap<>();

        // oneOf with two branches
        ComposedSchema oneOfSchema = new ComposedSchema();
        oneOfSchema.addOneOfItem(new StringSchema());
        oneOfSchema.addOneOfItem(new IntegerSchema());
        oneOfSchema.setDiscriminator(
                new io.swagger.v3.oas.models.media.Discriminator()
                        .propertyName("type"));
        schemas.put("OneOfTest", oneOfSchema);

        // anyOf with mixed branches
        ComposedSchema anyOfSchema = new ComposedSchema();
        anyOfSchema.addAnyOfItem(new StringSchema());
        anyOfSchema.addAnyOfItem(new NumberSchema());
        schemas.put("AnyOfTest", anyOfSchema);

        // allOf with property inheritance
        ComposedSchema allOfSchema = new ComposedSchema();
        ObjectSchema baseObj = new ObjectSchema();
        baseObj.addProperties("name", new StringSchema());
        allOfSchema.addAllOfItem(baseObj);
        schemas.put("AllOfTest", allOfSchema);

        // A schema can carry more than one composition keyword. The primary
        // descriptor selects representation, but every keyword is validated.
        ComposedSchema combinedSchema = new ComposedSchema();
        combinedSchema.addOneOfItem(new StringSchema());
        combinedSchema.addOneOfItem(new IntegerSchema());
        combinedSchema.addAllOfItem(new ObjectSchema().addProperties("id", new StringSchema()));
        schemas.put("CombinedCompositionTest", combinedSchema);

        // Schema without composition (should have no descriptor)
        schemas.put("SimpleModel", new ObjectSchema());

        components.setSchemas(schemas);
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);

        // Assert descriptors exist for composed schemas
        Oas31CompositionLowering.CompositionDescriptor oneOfDesc =
                codegen.getCompositionDescriptor("OneOfTest");
        Assert.assertNotNull(oneOfDesc, "OneOfTest should have a composition descriptor");
        Assert.assertEquals(oneOfDesc.getKeyword(), "oneOf",
                "Keyword must be lowercase string 'oneOf'");
        Assert.assertEquals(oneOfDesc.getBranches().size(), 2);
        Assert.assertEquals(oneOfDesc.getSchemaLocation(),
                "#/components/schemas/OneOfTest");

        // Discriminator must be captured
        Assert.assertNotNull(oneOfDesc.getDiscriminator(),
                "OneOfTest with discriminator must capture DiscriminatorDescriptor");
        Assert.assertEquals(oneOfDesc.getDiscriminator().getPropertyName(), "type");

        Oas31CompositionLowering.CompositionDescriptor anyOfDesc =
                codegen.getCompositionDescriptor("AnyOfTest");
        Assert.assertNotNull(anyOfDesc, "AnyOfTest should have a composition descriptor");
        Assert.assertEquals(anyOfDesc.getKeyword(), "anyOf",
                "Keyword must be lowercase string 'anyOf'");
        Assert.assertEquals(anyOfDesc.getBranches().size(), 2);

        Oas31CompositionLowering.CompositionDescriptor allOfDesc =
                codegen.getCompositionDescriptor("AllOfTest");
        Assert.assertNotNull(allOfDesc, "AllOfTest should have a composition descriptor");
        Assert.assertEquals(allOfDesc.getKeyword(), "allOf",
                "Keyword must be lowercase string 'allOf'");

        List<Oas31CompositionLowering.CompositionDescriptor> combinedDescriptors =
                codegen.getCompositionDescriptorsForSchema("CombinedCompositionTest");
        Assert.assertEquals(combinedDescriptors.size(), 2,
                "Combined schemas must retain every composition descriptor");
        Assert.assertEquals(combinedDescriptors.get(0).getKeyword(), "oneOf");
        Assert.assertEquals(combinedDescriptors.get(1).getKeyword(), "allOf");

        // SimpleModel should have NO descriptor
        Assert.assertNull(codegen.getCompositionDescriptor("SimpleModel"),
                "SimpleModel should not have a composition descriptor");

        // Preserve branch order
        Assert.assertEquals(oneOfDesc.getBranches().get(0).getBranchIndex(), 0);
        Assert.assertEquals(oneOfDesc.getBranches().get(1).getBranchIndex(), 1);
    }

    @Test
    public void buildsCompositionDescriptorWithRefResolutionAndCycleDetection() {
        // Verify that $ref branches are resolved with cycle detection
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        openAPI.setServers(new java.util.ArrayList<>());
        io.swagger.v3.oas.models.Components components =
                new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new java.util.LinkedHashMap<>();

        // Target schema for $ref
        schemas.put("TargetModel", new StringSchema());

        // oneOf with $ref branch
        ComposedSchema refOneOf = new ComposedSchema();
        Schema refBranch = new Schema();
        refBranch.set$ref("#/components/schemas/TargetModel");
        refOneOf.addOneOfItem(refBranch);
        refOneOf.addOneOfItem(new IntegerSchema());
        schemas.put("RefOneOf", refOneOf);

        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor descriptor =
                codegen.getCompositionDescriptor("RefOneOf");
        Assert.assertNotNull(descriptor);
        Assert.assertEquals(descriptor.getBranches().size(), 2);

        // First branch should have $ref recorded
        Oas31CompositionLowering.CompositionBranchDescriptor refBranchDesc =
                descriptor.getBranches().get(0);
        Assert.assertEquals(refBranchDesc.getSourceSchemaRef(),
                "#/components/schemas/TargetModel");
        Assert.assertEquals(refBranchDesc.getResolvedSchemaName(), "TargetModel");
        Assert.assertEquals(refBranchDesc.getNullCapability(),
                Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.NEVER);

        // Assertion metadata must be present on the resolved $ref target
        Assert.assertTrue(refBranchDesc.getSupportedAssertions().contains("type"),
                "$ref branch must capture 'type' assertion from resolved target");
        Assert.assertTrue(refBranchDesc.getUnsupportedAssertions().isEmpty(),
                "StringSchema should have no unsupported assertions");
    }

    @Test
    public void resetsCompositionStateForEveryGeneratorRun() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI first = new io.swagger.v3.oas.models.OpenAPI();
        first.setOpenapi("3.1.0");
        first.setServers(new java.util.ArrayList<>());
        io.swagger.v3.oas.models.Components components =
                new io.swagger.v3.oas.models.Components();
        ComposedSchema composed = new ComposedSchema();
        composed.addOneOfItem(new StringSchema());
        composed.addOneOfItem(new IntegerSchema());
        Map<String, Schema> schemas = new java.util.LinkedHashMap<>();
        schemas.put("FirstRunModel", composed);
        components.setSchemas(schemas);
        first.setComponents(components);

        codegen.preprocessOpenAPI(first);
        Map<String, Oas31CompositionLowering.CompositionDescriptor> firstRun =
                codegen.getCompositionDescriptors();
        Assert.assertTrue(firstRun.containsKey("FirstRunModel"));

        io.swagger.v3.oas.models.OpenAPI second = new io.swagger.v3.oas.models.OpenAPI();
        second.setOpenapi("3.1.0");
        second.setServers(new java.util.ArrayList<>());
        codegen.preprocessOpenAPI(second);

        Assert.assertTrue(codegen.getCompositionDescriptors().isEmpty(),
                "A reused generator must not expose descriptors from its prior run");
        Assert.assertTrue(firstRun.containsKey("FirstRunModel"),
                "Each run must own a distinct descriptor map");
    }

    @Test
    public void isolatesCompositionStateAcrossParallelGeneratorInstances() throws Exception {
        java.util.concurrent.ExecutorService executor =
                java.util.concurrent.Executors.newFixedThreadPool(4);
        try {
            List<java.util.concurrent.Callable<Boolean>> tasks = new java.util.ArrayList<>();
            for (int index = 0; index < 8; ++index) {
                String modelName = "ParallelModel" + index;
                tasks.add(() -> {
                    CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
                    codegen.processOpts();
                    io.swagger.v3.oas.models.OpenAPI openAPI =
                            new io.swagger.v3.oas.models.OpenAPI();
                    openAPI.setOpenapi("3.1.0");
                    openAPI.setServers(new java.util.ArrayList<>());
                    io.swagger.v3.oas.models.Components components =
                            new io.swagger.v3.oas.models.Components();
                    ComposedSchema composed = new ComposedSchema();
                    composed.addOneOfItem(new StringSchema());
                    composed.addOneOfItem(new IntegerSchema());
                    Map<String, Schema> schemas = new java.util.LinkedHashMap<>();
                    schemas.put(modelName, composed);
                    components.setSchemas(schemas);
                    openAPI.setComponents(components);

                    codegen.preprocessOpenAPI(openAPI);
                    return codegen.getCompositionDescriptors().keySet().equals(
                            Collections.singleton(modelName));
                });
            }
            for (java.util.concurrent.Future<Boolean> result : executor.invokeAll(tasks)) {
                Assert.assertTrue(result.get(),
                        "Parallel generator instances must not share composition state");
            }
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    public void compositionDescriptorsSurviveFullPipeline() throws IOException {
        // Contract test: descriptors built in preprocessOpenAPI survive
        // the full generation pipeline (normalization → inline flattening
        // → preprocessOpenAPI → fromModel → postProcessModels).
        // Verifies descriptor-driven lowering produces correct C++ types
        // in the final generated output.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        File output = java.nio.file.Files.createTempDirectory(
                "cpp-boost-beast-desc-fullpipe").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(
                        "src/test/resources/3_1/cpp-boost-beast-client/composed-schema-lowering.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "DescriptorPipelineTest");

        // Full pipeline via DefaultGenerator
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Contract: descriptor-driven lowering must produce correct types
        //
        // InputParam (oneOf string + array) → std::variant<std::string, std::vector<InputItem>>
        Path inputParam = output.toPath().resolve("model/InputParam.h");
        Assert.assertTrue(java.nio.file.Files.exists(inputParam),
                "InputParam (oneOf) must generate a model header");
        String inputParamContent = java.nio.file.Files.readString(inputParam);
        Assert.assertTrue(inputParamContent.contains("std::variant<")
                        && inputParamContent.contains("std::string")
                        && inputParamContent.contains("std::vector<InputItem>"),
                "InputParam must lower to std::variant<std::string, std::vector<InputItem>>; content: "
                        + inputParamContent.substring(0, Math.min(500, inputParamContent.length())));

        // OptionalScore (oneOf [null, number]) → std::optional<double>
        Path optionalScore = output.toPath().resolve("model/OptionalScore.h");
        Assert.assertTrue(java.nio.file.Files.exists(optionalScore),
                "OptionalScore (oneOf null+number) must generate a model header");
        String optionalScoreContent = java.nio.file.Files.readString(optionalScore);
        Assert.assertTrue(optionalScoreContent.contains("std::optional"),
                "OptionalScore must lower to std::optional<double>; content: "
                        + optionalScoreContent.substring(0, Math.min(500, optionalScoreContent.length())));

        // ModelIdsShared keeps both anyOf string branches tagged because the
        // enum branch has different validation semantics from plain string.
        Path modelIds = output.toPath().resolve("model/ModelIdsShared.h");
        Assert.assertTrue(java.nio.file.Files.exists(modelIds),
                "ModelIdsShared (anyOf) must generate a model header");
        String modelIdsContent = java.nio.file.Files.readString(modelIds);
        Assert.assertTrue(modelIdsContent.contains(
                        "std::variant<CompositionBranchValue<0, std::string>, "
                                + "CompositionBranchValue<1, std::string>>"),
                "ModelIdsShared must preserve each anyOf branch identity; content: "
                        + modelIdsContent.substring(0, Math.min(500, modelIdsContent.length())));

        // PetByType (oneOf with discriminator) → std::variant<Cat, Dog> or similar
        Path petByType = output.toPath().resolve("model/PetByType.h");
        Assert.assertTrue(java.nio.file.Files.exists(petByType),
                "PetByType (oneOf with discriminator) must generate a model header");
        String petByTypeContent = java.nio.file.Files.readString(petByType);
        Assert.assertTrue(petByTypeContent.contains("std::variant"),
                "PetByType must lower to variant type; content: "
                        + petByTypeContent.substring(0, Math.min(500, petByTypeContent.length())));
    }

    @Test
    public void normalizerPreservesCompositionBeforeDescriptorBuild()
            throws IOException {
        // Contract test: after normalization runs during DefaultGenerator,
        // the schema tree retains all original oneOf/anyOf branches so that
        // preprocessOpenAPI can build complete descriptors. Generate from
        // the full fixture and verify the descriptor index by checking
        // generated output reflects descriptor-driven lowering.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        File output = java.nio.file.Files.createTempDirectory(
                "cpp-boost-beast-norm-before-desc").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(
                        "src/test/resources/3_1/cpp-boost-beast-client/composed-schema-lowering.yaml")
                .setOutputDir(output.getAbsolutePath());

        // Run full pipeline so normalization runs before descriptor building
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify descriptor-driven lowering by checking generated output types.
        // All models in the fixture must produce correct lowering:
        Path dedupTest = output.toPath().resolve("model/DedupTest.h");
        Assert.assertTrue(java.nio.file.Files.exists(dedupTest),
                "DedupTest must generate a model header");
        String dedupContent = java.nio.file.Files.readString(dedupTest);
        // DedupTest (oneOf string-enum + integer + string) — two branches are
        // both std::string. The CompositionBranchValue wrappers preserve identity.
        Assert.assertTrue(dedupContent.contains("CompositionBranchValue"),
                "DedupTest must use CompositionBranchValue to preserve string "
                        + "branch identity; content: "
                        + dedupContent.substring(0, Math.min(500, dedupContent.length())));

        // Verify fromJsonValue uses descriptor-guided conversion
        Path dedupSource = output.toPath().resolve("model/DedupTest.cpp");
        Assert.assertTrue(java.nio.file.Files.exists(dedupSource),
                "DedupTest must generate a model source file");
        String dedupSourceContent = java.nio.file.Files.readString(dedupSource);
        Assert.assertTrue(dedupSourceContent.contains("matchedBranchIndex"),
                "DedupTest fromJsonValue must track matchedBranchIndex from "
                        + "validator (not tryVariantBranches); content: "
                        + dedupSourceContent.substring(0, Math.min(500, dedupSourceContent.length())));
        Assert.assertTrue(
                dedupSourceContent.contains("CompositionBranchValue<0, std::string>{std::move(converted)}"),
                "DedupTest fromJsonValue must construct CompositionBranchValue<0, "
                        + "std::string> from the converted branch value; content: "
                        + dedupSourceContent.substring(0, Math.min(500, dedupSourceContent.length())));

        // RefHolder must use the aliases referenced by its two properties.
        Path refHolder = output.toPath().resolve("model/RefHolder.h");
        Assert.assertTrue(java.nio.file.Files.exists(refHolder),
                "RefHolder must generate a model header");
        String refHolderContent = java.nio.file.Files.readString(refHolder);
        Assert.assertTrue(refHolderContent.contains("ModelIdsResponses"),
                "RefHolder must reference ModelIdsResponses for ids");
        Assert.assertTrue(refHolderContent.contains("InputParam"),
                "RefHolder must reference InputParam for param");
    }

    @Test
    public void normalizerBypassPreservesBranchCardinalityForOneOf() {
        // Direct test: verify that the normalizer's processSimplifyOneOf
        // returns the original schema unchanged when oneOf branches exist.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // Build a oneOf with branches that default normalizer would simplify
        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");

        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new StringSchema());
        schema.addOneOfItem(new IntegerSchema());
        schema.addOneOfItem(new NumberSchema());

        // Create the normalizer
        Map<String, String> rules = new HashMap<>();
        TestNormalizer normalizer =
                new TestNormalizer(openAPI, rules);

        Schema result = normalizer.processSimplifyOneOf(schema);
        Assert.assertNotNull(result);
        Assert.assertTrue(result.getOneOf() != null && result.getOneOf().size() == 3,
                "Normalizer must preserve original oneOf branch count");
    }

    @Test
    public void normalizerBypassPreservesBranchCardinalityForAnyOf() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");

        // anyOf with string + enum branch (default normalizer would simplify)
        ComposedSchema schema = new ComposedSchema();
        schema.addAnyOfItem(new StringSchema());
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("alpha");
        enumSchema.addEnumItem("beta");
        schema.addAnyOfItem(enumSchema);

        Map<String, String> rules = new HashMap<>();
        TestNormalizer normalizer =
                new TestNormalizer(openAPI, rules);

        // Test both processSimplifyAnyOf and processSimplifyAnyOfStringAndEnumString
        Schema anyOfResult = normalizer.processSimplifyAnyOf(schema);
        Assert.assertNotNull(anyOfResult);
        Assert.assertTrue(anyOfResult.getAnyOf() != null
                        && anyOfResult.getAnyOf().size() == 2,
                "processSimplifyAnyOf must preserve anyOf branch count");

        Schema stringEnumResult = normalizer.processSimplifyAnyOfStringAndEnumString(schema);
        Assert.assertNotNull(stringEnumResult);
        Assert.assertTrue(stringEnumResult.getAnyOf() != null
                        && stringEnumResult.getAnyOf().size() == 2,
                "processSimplifyAnyOfStringAndEnumString must preserve anyOf branch count");
    }

    @Test
    public void xCppCompositionBranchesStructureContract()
            throws Exception {
        // Contract test: validates the compiled branch structure is populated
        // on codegen state with correct keyword, branch count, and assertion
        // lists on each branch. Uses preprocessOpenAPI + fromModel +
        // postProcessModels to inspect descriptor-derived state.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        openAPI.setServers(new java.util.ArrayList<>());
        io.swagger.v3.oas.models.Components components =
                new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new java.util.LinkedHashMap<>();

        // oneOf with string + integer branches and a discriminator
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new StringSchema());
        schema.addOneOfItem(new IntegerSchema());
        schema.setDiscriminator(
                new io.swagger.v3.oas.models.media.Discriminator()
                        .propertyName("kind"));
        schemas.put("StringOrInt", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        // Descriptor must exist with correct keyword and branch count
        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("StringOrInt");
        Assert.assertNotNull(desc,
                "StringOrInt must have a composition descriptor");
        Assert.assertEquals(desc.getKeyword(), "oneOf",
                "composition descriptor keyword must be 'oneOf'");
        Assert.assertEquals(desc.getBranches().size(), 2,
                "composition descriptor must have 2 branches");

        // Each branch must have resolved-schema-name and supported assertions
        Oas31CompositionLowering.CompositionBranchDescriptor branch0 =
                desc.getBranches().get(0);
        Assert.assertEquals(branch0.getResolvedSchemaName(), "string",
                "Branch 0 must be the string branch");
        Assert.assertTrue(
                branch0.getSupportedAssertions().contains("type"),
                "String branch must have 'type' in supportedAssertions");

        Oas31CompositionLowering.CompositionBranchDescriptor branch1 =
                desc.getBranches().get(1);
        Assert.assertEquals(branch1.getResolvedSchemaName(), "integer",
                "Branch 1 must be the integer branch");
        Assert.assertTrue(
                branch1.getSupportedAssertions().contains("type"),
                "Integer branch must have 'type' in supportedAssertions");

        // Discriminator must be present
        Assert.assertTrue(desc.hasDiscriminator(),
                "Descriptor must have discriminator");
        Assert.assertEquals(desc.getDiscriminator().getPropertyName(), "kind",
                "Discriminator property name must be 'kind'");

        // Run lowering and verify the composition-branches extension survives
        CodegenModel cm = codegen.fromModel("StringOrInt", schema);
        if (cm.classname == null) {
            cm.classname = "StringOrInt";
        }
        org.openapitools.codegen.model.ModelsMap modelsMap =
                new org.openapitools.codegen.model.ModelsMap();
        org.openapitools.codegen.model.ModelMap modelWrap =
                new org.openapitools.codegen.model.ModelMap();
        modelWrap.setModel(cm);
        java.util.List<org.openapitools.codegen.model.ModelMap> modelList =
                new java.util.ArrayList<>();
        modelList.add(modelWrap);
        modelsMap.setModels(modelList);
        modelsMap = codegen.postProcessModels(modelsMap);

        // After lowering, the composition-branches extension must still be present
        CodegenModel processed = modelsMap.getModels().get(0).getModel();
        Object extValue = processed.vendorExtensions.get("x-cpp-composition-branches");
        Assert.assertNotNull(extValue,
                "composition-branches extension must survive postProcessModels");
        @SuppressWarnings("unchecked")
        Map<String, Object> extMap = (Map<String, Object>) extValue;
        Assert.assertEquals(extMap.get("keyword"), "oneOf",
                "composition descriptor keyword must be 'oneOf'");
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> branches =
                (List<Map<String, Object>>) extMap.get("branches");
        Assert.assertNotNull(branches, "composition-branches extension must have branches");
        Assert.assertEquals(branches.size(), 2,
                "composition descriptor must have 2 branches");

        // Each branch map must have assertion and capability fields
        for (Map<String, Object> brMap : branches) {
            Assert.assertTrue(brMap.containsKey("branch-index"),
                    "Branch must have branch-index");
            Assert.assertTrue(brMap.containsKey("null-capability"),
                    "Branch must have null-capability");
            Assert.assertTrue(brMap.containsKey("supported-assertions"),
                    "Branch must have supported-assertions");
            Assert.assertTrue(brMap.containsKey("unsupported-assertions"),
                    "Branch must have unsupported-assertions");
        }
    }

    @Test
    public void descriptorDrivesLoweringMetadata() {
        // Contract test: processComposedModel/lowerComposedTypes must read
        // the CompositionDescriptor when available, using its nullCapability
        // metadata for Rule 1 ([T, null] → optional<T>) instead of inferring
        // from C++ type strings alone. Verify descriptor is looked up by
        // toModelName.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        openAPI.setServers(new java.util.ArrayList<>());
        io.swagger.v3.oas.models.Components components =
                new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new java.util.LinkedHashMap<>();

        // oneOf with null, discriminator, and string branches
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new StringSchema());
        Schema nullBranch = new Schema();
        nullBranch.set$ref("#/components/schemas/NullModel");
        schema.addOneOfItem(nullBranch);
        schema.setDiscriminator(
                new io.swagger.v3.oas.models.media.Discriminator()
                        .propertyName("type"));
        schemas.put("StringOrNull", schema);
        schemas.put("NullModel", new Schema().nullable(true));

        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        // Descriptor must be indexed by toModelName
        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("StringOrNull");
        Assert.assertNotNull(desc, "StringOrNull must have a descriptor by toModelName");
        Assert.assertEquals(desc.getKeyword(), "oneOf",
                "Keyword must be 'oneOf' not 'ONE_OF'");
        Assert.assertEquals(desc.getBranches().size(), 2,
                "Branch count must be preserved");

        // Null branch must detect null capability from the $ref target
        Oas31CompositionLowering.CompositionBranchDescriptor nullBranchDesc =
                desc.getBranches().get(1);
        Assert.assertTrue(
                nullBranchDesc.getNullCapability()
                        == Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.ALWAYS
                || nullBranchDesc.getNullCapability()
                        == Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.CONDITIONAL,
                "Null $ref branch must have ALWAYS or CONDITIONAL nullCapability, got: "
                        + nullBranchDesc.getNullCapability());

        // Discriminator must be captured
        Assert.assertTrue(desc.getDiscriminator() != null,
                "Descriptor must capture discriminator");
        Assert.assertEquals(desc.getDiscriminator().getPropertyName(), "type",
                "Discriminator property name must be captured");

        // Branch 0 is an inline StringSchema and must retain its type assertion.
        Oas31CompositionLowering.CompositionBranchDescriptor stringBranchDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(stringBranchDesc.getSupportedAssertions().contains("type"),
                "Inline string branch must have a type assertion in supported assertions");

        // Null branch must not have unsupported assertions (simple nullable ref)
        Assert.assertTrue(nullBranchDesc.getUnsupportedAssertions().isEmpty(),
                "Simple nullable $ref should have empty unsupportedAssertions");
    }

    @Test
    public void descriptorBranchIndexAlignsAfterSelfRefFiltering()
            throws Exception {
        // Contract test: when a self-referencing oneOf branch is filtered
        // in processComposedModel, lowerComposedTypes Rule 1 and Rule 3
        // must still correctly align descriptor nullCapability via
        // originalBranchIndex. Schema: oneOf [SelfModel, null, string].
        // Invokes full lowering (preprocessOpenAPI → fromModel →
        // postProcessModels) and checks the final vendor extension.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        openAPI.setServers(new java.util.ArrayList<>());
        io.swagger.v3.oas.models.Components components =
                new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new java.util.LinkedHashMap<>();

        // Self-referencing oneOf: SelfModel, null, string
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new Schema().$ref("#/components/schemas/SchemaWithSelfRef"));
        Schema nullBranch = new Schema();
        nullBranch.set$ref("#/components/schemas/NullType");
        schema.addOneOfItem(nullBranch);
        schema.addOneOfItem(new StringSchema());
        schemas.put("SchemaWithSelfRef", schema);
        schemas.put("NullType", new Schema().nullable(true));

        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        // Step 1: Verify descriptor has correct structure
        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithSelfRef");
        Assert.assertNotNull(desc,
                "SchemaWithSelfRef must have a composition descriptor");
        Assert.assertEquals(desc.getBranches().size(), 3,
                "Descriptor must have 3 branches (self-ref, null, string)");

        // Step 2: Run lowering via fromModel + postProcessModels
        // fromModel converts the raw schema into a CodegenModel with
        // composedSchemas containing oneOf CodegenProperty branches.
        CodegenModel cm = codegen.fromModel("SchemaWithSelfRef", schema);
        Assert.assertNotNull(cm, "fromModel must produce a CodegenModel");
        // Set classname explicitly if fromModel didn't
        if (cm.classname == null) {
            cm.classname = "SchemaWithSelfRef";
        }

        // Wrap in ModelsMap for postProcessModels
        org.openapitools.codegen.model.ModelsMap modelsMap =
                new org.openapitools.codegen.model.ModelsMap();
        org.openapitools.codegen.model.ModelMap modelWrap =
                new org.openapitools.codegen.model.ModelMap();
        modelWrap.setModel(cm);
        java.util.List<org.openapitools.codegen.model.ModelMap> modelList =
                new java.util.ArrayList<>();
        modelList.add(modelWrap);
        modelsMap.setModels(modelList);
        modelsMap = codegen.postProcessModels(modelsMap);

        // Step 3: Verify lowering results in correct type
        CodegenModel processed = modelsMap.getModels().get(0).getModel();
        Assert.assertTrue(processed.vendorExtensions.containsKey("x-cpp-type"),
                "SchemaWithSelfRef must carry the resolved-type extension after lowering");
        String resolvedType = (String) processed.vendorExtensions.get("x-cpp-type");
        // After self-ref filtering: composed branches = [null (idx=1), string (idx=2)].
        // Rule 1 via descriptor: alwaysNullCount=1, branches.size()==2 →
        // "std::optional<std::string>".
        Assert.assertTrue(resolvedType != null
                        && resolvedType.contains("std::optional")
                        && resolvedType.contains("std::string"),
                "SchemaWithSelfRef must lower to std::optional<std::string> "
                        + "(self-ref filtered, Rule 1 detects [null, T] pattern via descriptor), got: "
                        + resolvedType);

        // Verify the branch-original-index extension contains the descriptor positions
        // after the self-ref (branch 0) was filtered: [1, 2]
        Assert.assertTrue(processed.vendorExtensions
                        .containsKey("x-cpp-branch-original-index"),
                "SchemaWithSelfRef must retain the branch-original-index extension");
        @SuppressWarnings("unchecked")
        List<Integer> storedIndices = (List<Integer>) processed.vendorExtensions
                .get("x-cpp-branch-original-index");
        Assert.assertNotNull(storedIndices,
                "branch-original-index extension must not be null");
        Assert.assertEquals(storedIndices.size(), 2,
                "branch-original-index must have 2 branches after self-ref skip");
        Assert.assertEquals((int) storedIndices.get(0), 1,
                "First composed branch (null) must have originalBranchIndex=1");
        Assert.assertEquals((int) storedIndices.get(1), 2,
                "Second composed branch (string) must have originalBranchIndex=2");
    }

    @Test
    public void discriminatorBranchIndexMappingBuilt() {
        // Contract test: x-discriminator-branch-index must be built from
        // discriminator mapping values matched against branch resolved schema
        // names as a List<{key, value}> for Mustache iteration.
        // URI-style and plain-name mappings both work; unresolvable mappings
        // cause a generation diagnostic (RuntimeException).
        //
        // MappedModel overload: resolves via schemaName (raw, handles lowercase)
        // AND modelName (sanitized) against branch resolvedSchemaName.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        openAPI.setServers(new java.util.ArrayList<>());
        io.swagger.v3.oas.models.Components components =
                new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new java.util.LinkedHashMap<>();

        // Animal schema: oneOf with Mammal, Bird, discriminator with explicit URI mapping
        ComposedSchema animal = new ComposedSchema();
        Schema mammalRef = new Schema().$ref("#/components/schemas/Mammal");
        Schema birdRef = new Schema().$ref("#/components/schemas/Bird");
        animal.addOneOfItem(mammalRef);
        animal.addOneOfItem(birdRef);
        animal.setDiscriminator(
                new io.swagger.v3.oas.models.media.Discriminator()
                        .propertyName("kind")
                        .mapping("mammal", "#/components/schemas/Mammal")
                        .mapping("bird", "Bird"));
        schemas.put("Animal", animal);
        schemas.put("Mammal", new ObjectSchema());
        schemas.put("Bird", new ObjectSchema());

        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        // Descriptor must capture discriminator with mapping
        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("Animal");
        Assert.assertNotNull(desc, "Animal must have composition descriptor");
        Assert.assertTrue(desc.hasDiscriminator(), "Animal must have discriminator");
        Assert.assertEquals(desc.getDiscriminator().getPropertyName(), "kind");
        Assert.assertEquals(desc.getDiscriminator().getMapping().size(), 2,
                "Discriminator must have 2 mapping entries");

        // Build from explicit mapping (Map-based overload)
        List<Map<String, Object>> indexList =
                Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                        desc.getDiscriminator().getMapping(), desc.getBranches());
        Assert.assertNotNull(indexList,
                "buildDiscriminatorBranchIndex(Map) must return non-null list");
        Assert.assertEquals(indexList.size(), 2,
                "Both mammal and bird mappings must resolve to branches");
        // Each entry must have key and value; order matches the mapping insertion
        Assert.assertEquals(indexList.get(0).get("key"), "mammal",
                "First entry key must be 'mammal'");
        Assert.assertEquals(indexList.get(0).get("value"), 0,
                "mammal mapping must resolve to branch 0 (Mammal)");
        Assert.assertEquals(indexList.get(1).get("key"), "bird",
                "Second entry key must be 'bird'");
        Assert.assertEquals(indexList.get(1).get("value"), 1,
                "bird mapping must resolve to branch 1 (Bird)");

        // Test unresolvable mapping: extra mapping pointing to a non-existent schema
        // must throw RuntimeException (hard diagnostic).
        Map<String, String> mappingWithExtra = new java.util.LinkedHashMap<>();
        mappingWithExtra.put("mammal", "#/components/schemas/Mammal");
        mappingWithExtra.put("bird", "Bird");
        mappingWithExtra.put("reptile", "Reptile"); // not in branches
        boolean threwExpected = false;
        try {
            Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                    mappingWithExtra, desc.getBranches());
        } catch (RuntimeException re) {
            threwExpected = true;
            String message = re.getMessage();
            Assert.assertTrue(
                    message.contains("reptile")
                    || message.contains("Reptile")
                    || message.contains("does not match"),
                    "Unresolvable mapping diagnostic must reference unresolvable entry; got: "
                    + message);
        }
        Assert.assertTrue(threwExpected,
                "buildDiscriminatorBranchIndex must throw for unresolvable mappings");

        // Test MappedModel overload: must resolve via schemaName (raw name) AND
        // modelName (sanitized name) to handle lowercase/raw schema names.
        Set<org.openapitools.codegen.CodegenDiscriminator.MappedModel> mappedModels =
                new java.util.LinkedHashSet<>();
        // MappedModel with lowercase schemaName "cat" matching resolvedSchemaName "cat"
        mappedModels.add(new org.openapitools.codegen.CodegenDiscriminator.MappedModel(
                "feline", "Cat", "cat", false));
        // MappedModel with uppercase schemaName that matches modelName
        mappedModels.add(new org.openapitools.codegen.CodegenDiscriminator.MappedModel(
                "canine", "Dog", null, false));

        List<Oas31CompositionLowering.CompositionBranchDescriptor> testBranches =
                new java.util.ArrayList<>();
        // Branch 0: resolvedSchemaName = "cat" (lowercase, matches schemaName)
        testBranches.add(new Oas31CompositionLowering.CompositionBranchDescriptor(
                0, "#/components/schemas/cat", "cat", "Cat",
                "validate_cat_branch_0",
                Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.NEVER,
                java.util.Collections.emptyList(),
                java.util.Collections.emptyList(),
                java.util.Collections.emptyMap()));
        // Branch 1: resolvedSchemaName = "Dog" (uppercase, matches modelName)
        testBranches.add(new Oas31CompositionLowering.CompositionBranchDescriptor(
                1, "#/components/schemas/Dog", "Dog", "Dog",
                "validate_dog_branch_1",
                Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.NEVER,
                java.util.Collections.emptyList(),
                java.util.Collections.emptyList(),
                java.util.Collections.emptyMap()));

        // Build via MappedModel overload
        List<Map<String, Object>> mmIndex =
                Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                        mappedModels, testBranches);
        Assert.assertNotNull(mmIndex,
                "buildDiscriminatorBranchIndex(MappedModel) must return non-null list");
        Assert.assertEquals(mmIndex.size(), 2,
                "Both MappedModel entries must resolve to branches");
        // First entry: feline → cat (resolved via schemaName)
        Assert.assertEquals(mmIndex.get(0).get("key"), "feline",
                "First entry key must be 'feline'");
        Assert.assertEquals(mmIndex.get(0).get("value"), 0,
                "feline mapping must resolve to branch 0 via schemaName match");
        // Second entry: canine → Dog (resolved via modelName)
        Assert.assertEquals(mmIndex.get(1).get("key"), "canine",
                "Second entry key must be 'canine'");
        Assert.assertEquals(mmIndex.get(1).get("value"), 1,
                "canine mapping must resolve to branch 1 via modelName match");
    }

    @Test
    public void descriptorUnsupportedAssertionsPopulated() {
        // Contract test: CompositionBranchDescriptor.unsupportedAssertions
        // must be populated with known-unsupported keywords when present
        // in the resolved schema.
        // Conditional (if/then/else) is no longer fail-closed (emitted as
        // validation-if/then/else-schema); `contains` remains unsupported and
        // must still stop oneOf generation fail-closed.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        openAPI.setServers(new java.util.ArrayList<>());
        io.swagger.v3.oas.models.Components components =
                new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new java.util.LinkedHashMap<>();

        // Schema with conditional (supported) + contains (supported) +
        // contentEncoding (annotation-only per 2020-12 §8.2.6 — supported,
        // no longer fail-closed)
        ComposedSchema schema = new ComposedSchema();
        StringSchema conditionalBranch = new StringSchema();
        conditionalBranch.setMinLength(1);
        io.swagger.v3.oas.models.media.Schema ifSchema =
                new io.swagger.v3.oas.models.media.Schema();
        ifSchema.setType("object");
        conditionalBranch.setIf(ifSchema);
        conditionalBranch.setThen(new Schema());
        schema.addOneOfItem(conditionalBranch);

        ArraySchema arrayWithContains = new ArraySchema();
        arrayWithContains.setContains(new StringSchema());
        arrayWithContains.setItems(new StringSchema());
        schema.addOneOfItem(arrayWithContains);

        StringSchema contentEncoded = new StringSchema();
        contentEncoded.setContentEncoding("base64");
        schema.addOneOfItem(contentEncoded);

        schemas.put("SchemaWithUnsupported", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        // Every keyword in this fixture is now supported — conditional
        // (if/then/else), contains (with its count bounds), and
        // content-encoding (annotation-only per 2020-12 §8.2.6, it can never
        // affect composition membership). preprocessOpenAPI must NOT throw.
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("SchemaWithUnsupported");
        Assert.assertNotNull(desc,
                "SchemaWithUnsupported must have a descriptor");
        // The conditional branch (branch 0) must NOT list conditional as
        // unsupported; the if schema must be surfaced for IR emission.
        Oas31CompositionLowering.CompositionBranchDescriptor conditionalBranchDesc =
                desc.getBranches().get(0);
        Assert.assertFalse(
                conditionalBranchDesc.getUnsupportedAssertions().contains("conditional"),
                "conditional must be supported (emitted as validation-if-schema)");
        Assert.assertNotNull(
                conditionalBranchDesc.getValidateParams().get("validation-if-schema"),
                "validation-if-schema must be surfaced for the conditional branch");
        // The contains branch (branch 1) must be SUPPORTED: its subschema is
        // surfaced for IR emission.
        Oas31CompositionLowering.CompositionBranchDescriptor containsBranchDesc =
                desc.getBranches().get(1);
        Assert.assertFalse(
                containsBranchDesc.getUnsupportedAssertions().contains("contains"),
                "contains must be supported");
        Assert.assertNotNull(
                containsBranchDesc.getValidateParams().get("validation-contains-schema"),
                "validation-contains-schema must be surfaced for the contains branch");
        // The contentEncoding branch (branch 2) must now be SUPPORTED:
        // annotation-only keyword — collected, never fail-closed.
        Oas31CompositionLowering.CompositionBranchDescriptor encodedBranchDesc =
                desc.getBranches().get(2);
        Assert.assertTrue(encodedBranchDesc.getSupportedAssertions().contains("content-encoding"),
                "contentEncoding must be supported (annotation surface)");
        Assert.assertFalse(
                encodedBranchDesc.getUnsupportedAssertions().contains("content-encoding"),
                "contentEncoding must NOT be fail-closed");
    }
}
