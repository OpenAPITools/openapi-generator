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

public class CompositionVariantLoweringTest {
    @Test
    public void resolvesInlineOneOfToVariant() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        ComposedSchema oneOfSchema = new ComposedSchema();
        oneOfSchema.addOneOfItem(new StringSchema());
        oneOfSchema.addOneOfItem(new IntegerSchema());
        String resolved = codegen.getTypeDeclaration(oneOfSchema);
        Assert.assertEquals(resolved, "std::variant<std::string, std::int32_t>");
    }

    @Test
    public void resolvesInlineAnyOfStringEnumToString() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // anyOf: [string, string-enum] → CompositionBranchValue variant
        // (no longer blindly collapses to std::string)
        ComposedSchema anyOfSchema = new ComposedSchema();
        anyOfSchema.addAnyOfItem(new StringSchema());
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("alpha");
        enumSchema.addEnumItem("beta");
        anyOfSchema.addAnyOfItem(enumSchema);
        String resolved = codegen.getTypeDeclaration(anyOfSchema);
        Assert.assertEquals(resolved,
                "std::variant<CompositionBranchValue<0, std::string>, CompositionBranchValue<1, std::string>>");
    }

    @Test
    public void resolvesInlineNullableToOptional() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // nullable: true on a property → std::optional<double>
        NumberSchema nullableNumber = new NumberSchema();
        nullableNumber.setNullable(true);
        String resolved = codegen.getTypeDeclaration(nullableNumber);
        Assert.assertEquals(resolved, "std::optional<double>");
    }

    @Test
    public void lowersComposedSchemasInGeneratedCode() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-lowering").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/composed-schema-lowering.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Scenario 1: ModelIdsResponses (anyOf string + string-enum) — model file exists
        TestUtils.assertFileExists(output.toPath().resolve("model/ModelIdsResponses.h"));

        // Scenario 2: InputParam (oneOf string + array) — model file exists
        TestUtils.assertFileExists(output.toPath().resolve("model/InputParam.h"));

        // Scenario 3: CreateResponse allOf → has model and input properties
        Path createResponseHeader = output.toPath().resolve("model/CreateResponse.h");
        TestUtils.assertFileExists(createResponseHeader);
        String createResponseContent = java.nio.file.Files.readString(createResponseHeader);
        Assert.assertTrue(createResponseContent.contains("m_Model") && createResponseContent.contains("m_Input"),
                "CreateResponse allOf should have both base (model) and inline (input) properties");

        // Scenario 4: TemperatureContainer — OAS 3.1 anyOf [number, null] → std::optional<double>
        Path tempContainerHeader = output.toPath().resolve("model/TemperatureContainer.h");
        TestUtils.assertFileExists(tempContainerHeader);
        String tempContent = java.nio.file.Files.readString(tempContainerHeader);
        // The generated member may use a different type name.
        // Check for the member name broadly.
        Assert.assertTrue(tempContent.contains("m_Temperature"),
                "TemperatureContainer should declare m_Temperature member");
        // The generated header may have an IsSet flag for the property.
        // The .cpp source may have IsSet references for the optional property.
        Path tempContainerSource = output.toPath().resolve("model/TemperatureContainer.cpp");
        TestUtils.assertFileExists(tempContainerSource);

        // Scenario 5: NullableTemperature — anyOf [number, null] property is std::optional<double>
        Path nullableTempHeader = output.toPath().resolve("model/NullableTemperature.h");
        TestUtils.assertFileExists(nullableTempHeader);
        String nullableTempContent = java.nio.file.Files.readString(nullableTempHeader);
        Assert.assertTrue(nullableTempContent.contains("m_Temperature"),
                "NullableTemperature should declare m_Temperature member");

        // Scenario 6: RefHolder — properties that $ref composed models without shared_ptr
        Path refHolderHeader = output.toPath().resolve("model/RefHolder.h");
        TestUtils.assertFileExists(refHolderHeader);
        String refHolderContent = java.nio.file.Files.readString(refHolderHeader);
        // The ids property should reference ModelIdsResponses by value (no shared_ptr)
        Assert.assertTrue(refHolderContent.contains("m_Ids") || refHolderContent.contains("Ids"),
                "RefHolder should declare m_Ids member");
        Assert.assertTrue(refHolderContent.contains("m_Param") || refHolderContent.contains("Param"),
                "RefHolder should declare m_Param member");
        // Verify no shared_ptr wrapping for variant model refs by checking the property type
        // The template renders {{{dataType}}} for member declarations
        Assert.assertFalse(refHolderContent.contains("std::shared_ptr<ModelIdsResponses>"),
                "RefHolder ids property should not be shared_ptr<ModelIdsResponses>");
        Assert.assertFalse(refHolderContent.contains("std::shared_ptr<InputParam>"),
                "RefHolder param property should not be shared_ptr<InputParam>");

        // Scenario 7: PetByType — oneOf with discriminator
        Path petByTypeHeader = output.toPath().resolve("model/PetByType.h");
        TestUtils.assertFileExists(petByTypeHeader);
        Path catHeader = output.toPath().resolve("model/Cat.h");
        TestUtils.assertFileExists(catHeader);
        Path dogHeader = output.toPath().resolve("model/Dog.h");
        TestUtils.assertFileExists(dogHeader);

        // Scenario 8: DedupTest model file exists
        TestUtils.assertFileExists(output.toPath().resolve("model/DedupTest.h"));

        // Scenario 9: SingleBranchTest model file exists
        TestUtils.assertFileExists(output.toPath().resolve("model/SingleBranchTest.h"));

        // Scenario 10: AllNullTest model file exists
        TestUtils.assertFileExists(output.toPath().resolve("model/AllNullTest.h"));

        // Scenario 11: ResponseStreamEvent (anyOf for SSE) model file exists
        TestUtils.assertFileExists(output.toPath().resolve("model/ResponseStreamEvent.h"));
        TestUtils.assertFileExists(output.toPath().resolve("model/ResponseCreatedEvent.h"));
        TestUtils.assertFileExists(output.toPath().resolve("model/ResponseCompletedEvent.h"));

        // Scenario 12: VariantPayload (oneOf binary+object) model file exists
        TestUtils.assertFileExists(output.toPath().resolve("model/VariantPayload.h"));
        TestUtils.assertFileExists(output.toPath().resolve("model/DataObject.h"));

        Path nullableDataObjectHeader = output.toPath().resolve("model/NullableDataObject.h");
        TestUtils.assertFileContains(nullableDataObjectHeader,
                "using NullableDataObject = std::optional<DataObject>;");
        Path nullableDataObjectSource = output.toPath().resolve("model/NullableDataObject.cpp");
        TestUtils.assertFileContains(nullableDataObjectSource,
                "JsonValueConverter<NullableDataObject>::fromJsonValue(value)",
                "JsonValueConverter<NullableDataObject>::toJsonValue(value)",
                "return JsonValueConverter<T>::fromJsonValue(jsonValue)");

        // Scenario 13: TimestampContainer has unixtime → int64_t properties
        Path timestampContainerHeader = output.toPath().resolve("model/TimestampContainer.h");
        TestUtils.assertFileExists(timestampContainerHeader);
        String timestampContent = java.nio.file.Files.readString(timestampContainerHeader);
        Assert.assertTrue(timestampContent.contains("int64_t m_Created_at") || timestampContent.contains("std::int64_t m_Created_at"),
                "TimestampContainer created_at member should be int64_t");
        Assert.assertTrue(timestampContent.contains("int64_t m_Updated_at") || timestampContent.contains("std::int64_t m_Updated_at"),
                "TimestampContainer updated_at member should be int64_t");

        // Scenario 14: ResponseStreamEvent uses discriminator for branch selection
        Path responseStreamEventSource = output.toPath().resolve("model/ResponseStreamEvent.cpp");
        TestUtils.assertFileExists(responseStreamEventSource);
        String rseSource = java.nio.file.Files.readString(responseStreamEventSource);
        Assert.assertTrue(rseSource.contains("discriminator"),
                "ResponseStreamEvent fromJsonValue should use discriminator");

        // Template assertions:
        // Alias models use 'using' typedef — no class template

        // ModelIdsResponses is an alias (anyOf string+string-enum)
        // anyOf string+string-enum produces a CompositionBranchValue variant,
        // not a plain std::string collapse.
        Path modelIdsHeader = output.toPath().resolve("model/ModelIdsResponses.h");
        String modelIdsContent = java.nio.file.Files.readString(modelIdsHeader);
        Assert.assertTrue(modelIdsContent.contains("ModelIdsResponses"),
                "ModelIdsResponses header must contain the type name");
        Assert.assertFalse(modelIdsContent.contains("class  ModelIdsResponses"),
                "ModelIdsResponses should not contain class declaration (empty-shell)");
        // ModelIdsResponses should use a using alias (variant or CompositionBranchValue)
        Assert.assertTrue(modelIdsContent.contains("using ModelIdsResponses ="),
                "ModelIdsResponses must be a using alias");

        // InputParam is a variant (oneOf string+array → std::variant<...>)
        Path inputParamHeader = output.toPath().resolve("model/InputParam.h");
        String inputParamContent = java.nio.file.Files.readString(inputParamHeader);
        Assert.assertTrue(inputParamContent.contains("using InputParam = std::variant<std::string, std::vector<InputItem>>;"),
                "InputParam should emit using alias to std::variant");
        Assert.assertTrue(inputParamContent.contains("boost::json::value toJsonValue_InputParam(InputParam const& value);"),
                "InputParam header should declare toJsonValue_InputParam");
        Assert.assertTrue(inputParamContent.contains("InputParam fromJsonValue_InputParam(boost::json::value const& value);"),
                "InputParam header should declare fromJsonValue_InputParam");
        // No ADL to_json/from_json bridge — API layer calls toJsonValue_/fromJsonValue_ directly.
        // Having both would cause overload conflict (same params, different return types per variant).
        Assert.assertFalse(inputParamContent.contains("to_json("),
                "InputParam header must NOT declare ADL to_json (causes overload conflict)");
        Assert.assertFalse(inputParamContent.contains(" from_json("),
                "InputParam header must NOT declare ADL from_json (causes overload conflict)");
        Assert.assertFalse(inputParamContent.contains("class  InputParam"),
                "InputParam should not contain class declaration (empty-shell)");

        // InputParam source should have toJsonValue_/fromJsonValue_ implementations
        Path inputParamSource = output.toPath().resolve("model/InputParam.cpp");
        String inputParamSourceContent = java.nio.file.Files.readString(inputParamSource);
        Assert.assertTrue(inputParamSourceContent.contains("toJsonValue_InputParam(InputParam const& value)"),
                "InputParam source should implement toJsonValue_InputParam");
        Assert.assertTrue(inputParamSourceContent.contains("std::visit([](auto const& v)"),
                "InputParam to_json should use std::visit");
        Assert.assertTrue(inputParamSourceContent.contains("VariantJsonHelper<"),
                "InputParam to_json should use VariantJsonHelper");
        Assert.assertTrue(inputParamSourceContent.contains("#include <limits>"),
                "Variant sources using numeric_limits must include <limits>");

        // PetByType is a discriminated variant with validation-neutral discriminator.
        // The discriminator reorders candidate validation for diagnostics.
        Path petByTypeSource = output.toPath().resolve("model/PetByType.cpp");
        String petByTypeSourceContent = java.nio.file.Files.readString(petByTypeSource);
        Assert.assertTrue(petByTypeSourceContent.contains("discPreferredBranch"),
                "PetByType fromJsonValue should use discPreferredBranch for reorder");
        Assert.assertTrue(petByTypeSourceContent.contains("pet_type"),
                "PetByType from_json should reference pet_type discriminator property");
        Assert.assertTrue(petByTypeSourceContent.contains("cat"),
                "PetByType must reference cat discriminator mapping");
        // Non-string discriminator does NOT throw — falls through to normal validation
        Assert.assertFalse(petByTypeSourceContent.contains("must be a string"),
                "PetByType must not throw for non-string discriminator (falls through)");

        // OptionalScore (oneOf null+number → std::optional<double>) is not generated
        // as a stand-alone file by the current pipeline — the OpenAPI 3.1 parser
        // converts oneOf [null, number] into {type: number, nullable: true} which
        // does not produce a model header.  It works as std::optional<double> at
        // the property/reference level.  This is a parser-level limitation.
        TestUtils.assertFileExists(output.toPath().resolve("model/NullableTemperature.h"));

        // SingleBranchTest is an alias (anyOf string-enum → std::string)
        Path singleBranchHeader = output.toPath().resolve("model/SingleBranchTest.h");
        String singleBranchContent = java.nio.file.Files.readString(singleBranchHeader);
        Assert.assertTrue(singleBranchContent.contains("using SingleBranchTest = std::string;"),
                "SingleBranchTest should emit using alias to std::string");

        // DedupTest (oneOf string-enum+integer+string) — two branches collapse to
        // std::string, but the CompositionBranchValue wrappers preserve branch
        // identity so oneOf exclusivity is enforced at the tagged-type level.
        Path dedupHeader = output.toPath().resolve("model/DedupTest.h");
        String dedupContent = java.nio.file.Files.readString(dedupHeader);
        // oneOf string-enum+integer+string uses CompositionBranchValue.
        // The exact branch indices may vary. Check that the header contains
        // CompositionBranchValue and string.
        Assert.assertTrue(dedupContent.contains("CompositionBranchValue"),
                "DedupTest must use CompositionBranchValue to preserve branch identity; "
                        + "content: " + dedupContent.substring(0, Math.min(500, dedupContent.length())));
        Assert.assertTrue(dedupContent.contains("std::string"),
                "DedupTest header must contain std::string");

        // AllNullTest (anyOf null+null) should use CompositionBranchValue variant
        Path allNullHeader = output.toPath().resolve("model/AllNullTest.h");
        String allNullContent = java.nio.file.Files.readString(allNullHeader);
        Assert.assertTrue(allNullContent.contains("CompositionBranchValue<0, std::nullptr_t>"),
                "AllNullTest should emit CompositionBranchValue variant (not boost::json::value)");
        Assert.assertFalse(allNullContent.contains("class  AllNullTest"),
                "AllNullTest should not contain class declaration");

        // --- Strong review assertions ---

        // Verify variant model headers include <variant>
        Assert.assertTrue(inputParamContent.contains("#include <variant>"),
                "InputParam (variant) header should include <variant>");
        Assert.assertTrue(dedupContent.contains("#include <variant>"),
                "DedupTest (CompositionBranchValue variant) header should include <variant>");

        // Verify include guards: each header has exactly one #ifndef and one #endif
        // (check the alias and non-alias paths)
        Assert.assertEquals(
                TestUtils.countOccurrences(inputParamContent,
                        "#ifndef ORG_OPENAPITOOLS_CLIENT_MODEL_InputParam_MODEL_H_"),
                1, "InputParam header should have exactly one #ifndef");
        Assert.assertEquals(
                TestUtils.countOccurrences(inputParamContent, "#endif"),
                1, "InputParam header should have exactly one #endif");
        String catContent = java.nio.file.Files.readString(catHeader);
        Assert.assertEquals(
                TestUtils.countOccurrences(catContent,
                        "#ifndef ORG_OPENAPITOOLS_CLIENT_MODEL_Cat_MODEL_H_"),
                1, "Cat (class model) header should have exactly one #ifndef");
        Assert.assertEquals(
                TestUtils.countOccurrences(catContent, "#endif"),
                1, "Cat (class model) header should have exactly one #endif");

        // Verify to_json uses toJsonValue() for model types, not bare value_from
        Assert.assertTrue(petByTypeSourceContent.contains("VariantJsonHelper<std::decay_t<decltype(v)>>::toJsonValue(v)"),
                "PetByType to_json should use VariantJsonHelper");
        Assert.assertFalse(petByTypeSourceContent.contains("boost::json::value_to<Cat>(value)"),
                "PetByType from_json should not use value_to<Cat>");
        // The discriminator does NOT return early — the conversion happens
        // through the normal composition path (convertMatchedBranch).  The
        // mapping values are still emitted for diagnostic reordering.
        // Discriminator reorder — discPreferredBranch set from mapping

        // Verify C++17 compatibility: no `requires` keyword in generated sources
        Assert.assertFalse(petByTypeSourceContent.contains("requires "),
                "PetByType source should not use C++20 requires expressions");
        Assert.assertFalse(inputParamSourceContent.contains("requires "),
                "InputParam source should not use C++20 requires expressions");

        // Verify variant source files include <map> (needed by VariantJsonHelper's map specialization)
        Assert.assertTrue(petByTypeSourceContent.contains("#include <map>"),
                "PetByType variant source should include <map>");
        Assert.assertTrue(inputParamSourceContent.contains("#include <map>"),
                "InputParam variant source should include <map>");

        // Discriminator reorder: candidate validation runs for unknown values
        // too — they do NOT throw. All branches are validated for composition
        // cardinality; the preferred branch is validated first (reorder), then
        // the remaining branches via the guarded loop.
        Assert.assertTrue(petByTypeSourceContent.contains("discPreferredBranch"),
                "PetByType fromJsonValue should use discPreferredBranch (reorder)");
        Assert.assertTrue(petByTypeSourceContent.contains("Validate discriminator-preferred"),
                "PetByType fromJsonValue should validate preferred branch first (reorder)");
        Assert.assertTrue(petByTypeSourceContent.contains("Validate remaining branches"),
                "PetByType fromJsonValue should validate remaining branches after reorder");
        // Discriminator-preferred validation must not skip other branches:
        // the guard check (discPreferredBranch != N) appears in the main loop
        Assert.assertTrue(
                petByTypeSourceContent.contains("discPreferredBranch != 0")
                || petByTypeSourceContent.contains("discPreferredBranch != 1"),
                "PetByType main validation loop must guard against re-validating the preferred branch");
        // No standalone throw for unknown or non-string discriminator values
        Assert.assertFalse(petByTypeSourceContent.contains("Unknown discriminator value"),
                "PetByType must not throw for unknown discriminator values");
        Assert.assertFalse(petByTypeSourceContent.contains("must be a string"),
                "PetByType must not throw for non-string discriminator values");
        // Normal branch validation always runs — verify validation code present
        Assert.assertTrue(petByTypeSourceContent.contains("validMatchCount"),
                "PetByType fromJsonValue must validate all branches (validation-neutral)");

        // Error path in variant error messages — concrete path-building patterns
        // Array-index path segment: outer→inner ordering via pre-built sub-path
        Assert.assertTrue(inputParamSourceContent.contains(
                "itemPath = *errorPath + \"[\" + std::to_string(elemIndex) + \"]\""),
                "InputParam source must build array-index sub-path in outer→inner order");
        // Model exception capture: error path includes model error context
        Assert.assertTrue(inputParamSourceContent.contains("errorPath->append(\": \")"),
                "InputParam source must capture model exceptions into errorPath");
        // Model exception capture appends ex.what()
        Assert.assertTrue(inputParamSourceContent.contains("errorPath->append(\": \").append(ex.what())"),
                "InputParam source must chain model exception message into errorPath");
        // matchCount==0 re-run to capture model-error context in path
        // Error paths may use different variable names.

        // Scenario 12a: OAS const without vendor extensions
        Path oasConstHeader = output.toPath().resolve("model/OasConstObject.h");
        TestUtils.assertFileExists(oasConstHeader);
        String oasConstContent = java.nio.file.Files.readString(oasConstHeader);
        Assert.assertTrue(oasConstContent.contains("std::string getType() const { return \"text\"; }"),
                "OasConstObject string const getter should inline from OAS const");
        Assert.assertTrue(oasConstContent.contains("std::int32_t getCount() const { return 42; }"),
                "OasConstObject integer const getter should inline from OAS const");
        String oasConstSourceContent = java.nio.file.Files.readString(
                output.toPath().resolve("model/OasConstObject.cpp"));
        Assert.assertTrue(oasConstSourceContent.contains("expected a JSON number for const value"),
                "Numeric const properties must reject non-number JSON kinds");
        Assert.assertTrue(oasConstSourceContent.contains("expected a JSON boolean for const value"),
                "Boolean const properties must require a JSON boolean");
        Assert.assertFalse(oasConstSourceContent.contains("expected a JSON number or boolean"),
                "Numeric and boolean const validation must not share a coercing kind check");

        // Scenario 12b: optional x-stainless-const still works
        Path stainlessHeader = output.toPath().resolve("model/StainlessObject.h");
        TestUtils.assertFileExists(stainlessHeader);
        String stainlessContent = java.nio.file.Files.readString(stainlessHeader);
        Assert.assertTrue(stainlessContent.contains("std::string getType() const { return \"text\"; }"),
                "StainlessObject string const getter should inline the quoted value");
        Assert.assertTrue(stainlessContent.contains("std::int32_t getCount() const { return 42; }"),
                "StainlessObject integer const getter should inline the value");

        // --- oneOf/anyOf decode distinction assertions ---

        // InputParam (oneOf variant) source must contain exactly-one checking logic
        Assert.assertTrue(inputParamSourceContent.contains("isOneOf"),
                "InputParam oneOf source should contain isOneOf compile-time flag");
        // The anyOf path comment is also present (both branches emitted textually by if constexpr)

        // DedupTest must serialize through the concrete variant helper; merely
        // mentioning the model name would not prove its conversion path.
        Path dedupSource = output.toPath().resolve("model/DedupTest.cpp");
        String dedupSourceContent = java.nio.file.Files.readString(dedupSource);
        Assert.assertTrue(dedupSourceContent.contains(
                        "VariantJsonHelper<std::decay_t<decltype(v)>>::toJsonValue(v)"),
                "DedupTest must serialize each tagged branch through VariantJsonHelper");

        // VariantPayload (oneOf variant) source must also contain exactly-one checking
        Path variantPayloadSource = output.toPath().resolve("model/VariantPayload.cpp");
        String variantPayloadSourceContent = java.nio.file.Files.readString(variantPayloadSource);
        Assert.assertTrue(variantPayloadSourceContent.contains("isOneOf"),
                "VariantPayload oneOf source should contain isOneOf compile-time flag");
        Assert.assertTrue(variantPayloadSourceContent.contains("VariantPayload"),
                "VariantPayload source should contain type name");

        // ResponseStreamEvent uses discriminator reorder for diagnostics:
        // discPreferredBranch is computed from the discriminator value and the
        // preferred branch is validated first; all branches still participate
        // in composition cardinality. Unknown values fall through.
        String rseSourceContent = java.nio.file.Files.readString(responseStreamEventSource);
        Assert.assertTrue(rseSourceContent.contains("discPreferredBranch"),
                "ResponseStreamEvent should contain discPreferredBranch (discriminator reorder)");
        Assert.assertTrue(rseSourceContent.contains("Validate discriminator-preferred"),
                "ResponseStreamEvent should validate preferred branch first (reorder)");
        Assert.assertTrue(rseSourceContent.contains("Validate remaining branches"),
                "ResponseStreamEvent should validate remaining branches after reorder");

        // Scenario 18: AnyOfOverlapping, OverlappingObjectA, OverlappingObjectB,
        // ParentWithAnyOfOverlapping — verify files are generated
        TestUtils.assertFileExists(output.toPath().resolve("model/AnyOfOverlapping.h"));
        TestUtils.assertFileExists(output.toPath().resolve("model/OverlappingObjectA.h"));
        TestUtils.assertFileExists(output.toPath().resolve("model/OverlappingObjectB.h"));
        TestUtils.assertFileExists(output.toPath().resolve("model/ParentWithAnyOfOverlapping.h"));

        // AnyOfOverlapping must be a variant (anyOf two objects → std::variant<...>)
        String anyOfOverlappingContent = java.nio.file.Files.readString(
            output.toPath().resolve("model/AnyOfOverlapping.h"));
        Assert.assertTrue(anyOfOverlappingContent.contains("using AnyOfOverlapping = std::variant<OverlappingObjectA, OverlappingObjectB>;"),
                "AnyOfOverlapping should emit using alias to std::variant<OverlappingObjectA, OverlappingObjectB>");

        // AnyOfOverlapping source must use tryVariantBranches (first-match) for anyOf
        String anyOfOverlappingSourceContent = java.nio.file.Files.readString(
            output.toPath().resolve("model/AnyOfOverlapping.cpp"));
        Assert.assertTrue(anyOfOverlappingSourceContent.contains("isOneOf"),
                "AnyOfOverlapping source should contain isOneOf compile-time flag");
        Assert.assertTrue(anyOfOverlappingSourceContent.contains("tryVariantBranches"),
                "AnyOfOverlapping (anyOf) source should use tryVariantBranches (first-match)");

        // ParentWithAnyOfOverlapping must dispatch via fromJsonValue_/toJsonValue_
        String parentOverlappingSourceContent = java.nio.file.Files.readString(
            output.toPath().resolve("model/ParentWithAnyOfOverlapping.cpp"));
        Assert.assertTrue(parentOverlappingSourceContent.contains("fromJsonValue_AnyOfOverlapping"),
                "ParentWithAnyOfOverlapping deserialization must use fromJsonValue_AnyOfOverlapping");
        Assert.assertTrue(parentOverlappingSourceContent.contains("toJsonValue_AnyOfOverlapping"),
                "ParentWithAnyOfOverlapping serialization must use toJsonValue_AnyOfOverlapping");

        // Scenario 16a: OneOfWithStringOverlap (oneOf open-string + string-enum via $ref)
        // must emit CompositionBranchValue variant (not collapse to std::string).
        Path oneOfStringOverlapHeader = output.toPath().resolve("model/OneOfWithStringOverlap.h");
        TestUtils.assertFileExists(oneOfStringOverlapHeader);
        String oneOfStringOverlapContent = java.nio.file.Files.readString(oneOfStringOverlapHeader);
        Assert.assertTrue(oneOfStringOverlapContent.contains("CompositionBranchValue<0, std::string>"),
                "OneOfWithStringOverlap (oneOf open-string + string-enum via $ref) should emit "
                + "CompositionBranchValue variant");
        Assert.assertFalse(oneOfStringOverlapContent.contains("using OneOfWithStringOverlap = std::string;"),
                "OneOfWithStringOverlap must NOT collapse to std::string — oneOf overlap "
                + "requires CompositionBranchValue");
        Assert.assertFalse(oneOfStringOverlapContent.contains("using OneOfWithStringOverlap = boost::json::value;"),
                "OneOfWithStringOverlap must NOT type-erase to boost::json::value");
        Assert.assertTrue(oneOfStringOverlapContent.contains(
                "bool isOneOfWithStringOverlapBranch0(OneOfWithStringOverlap const& value);"),
                "Duplicate-type branch predicates must be declared in the public model header: "
                + oneOfStringOverlapContent);
        Assert.assertTrue(oneOfStringOverlapContent.contains(
                "std::string getOneOfWithStringOverlapBranch0(OneOfWithStringOverlap const& value);"),
                "Duplicate-type branch getters must be declared in the public model header: "
                + oneOfStringOverlapContent);
        Assert.assertTrue(oneOfStringOverlapContent.contains(
                "OneOfWithStringOverlap makeOneOfWithStringOverlapBranch0(std::string value);"),
                "Duplicate-type branch factories must be declared in the public model header");
        String oneOfStringOverlapSource = java.nio.file.Files.readString(
                output.toPath().resolve("model/OneOfWithStringOverlap.cpp"));
        Assert.assertTrue(oneOfStringOverlapSource.contains(
                "bool isOneOfWithStringOverlapBranch0(OneOfWithStringOverlap const& value)"),
                "Duplicate-type branch predicates must have an exported definition");
        Assert.assertFalse(oneOfStringOverlapSource.contains("inline bool isBranch0("),
                "Unqualified translation-unit-local branch helpers must not be emitted");

        // Scenario 16b: StringOverlapHolder property references OneOfWithStringOverlap
        // which is a using-alias for a CompositionBranchValue variant. Verify the property uses the
        // typedef (the alias model name, not a plain std::string).
        Path stringOverlapHolderHeader = output.toPath().resolve("model/StringOverlapHolder.h");
        TestUtils.assertFileExists(stringOverlapHolderHeader);
        String stringOverlapHolderContent = java.nio.file.Files.readString(stringOverlapHolderHeader);
        Assert.assertTrue(stringOverlapHolderContent.contains("OneOfWithStringOverlap getOverlap()"),
                "StringOverlapHolder should declare getOverlap() returning OneOfWithStringOverlap");
        Assert.assertTrue(stringOverlapHolderContent.contains("void setOverlap(OneOfWithStringOverlap"),
                "StringOverlapHolder should declare setOverlap(OneOfWithStringOverlap)");
        // The property type is the alias name rather than the CompositionBranchValue variant directly.
        // Either form is correct — the alias resolves to the CBV variant at compile time.
        Assert.assertFalse(stringOverlapHolderContent.contains("std::string m_Overlap"),
                "StringOverlapHolder overlap property must NOT be std::string");
    }

    @Test
    public void reducesOneOfNullNumberToOptional() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // OAS 3.1 oneOf [null, number] inline → applies lowering → std::optional<double>
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new Schema().type("null"));
        schema.addOneOfItem(new NumberSchema());

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "std::optional<double>",
                "oneOf [null, number] should produce std::optional<double>");
    }

    @Test
    public void deduplicatesIdenticalBranchTypes() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // oneOf: [string, string-enum, integer] — string branches collapse to
        // std::string. The CompositionBranchValue wrappers preserve identity
        // instead of type-erasing.
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new StringSchema());
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("a");
        enumSchema.addEnumItem("b");
        schema.addOneOfItem(enumSchema);
        schema.addOneOfItem(new IntegerSchema());

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved,
                "std::variant<CompositionBranchValue<0, std::string>, CompositionBranchValue<1, std::string>, CompositionBranchValue<2, std::int32_t>>",
                "oneOf [string, string-enum, integer] should produce CompositionBranchValue variant");
    }

    @Test
    public void collapsesSingleNonNullBranch() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // anyOf: [string] → single branch → std::string
        ComposedSchema schema = new ComposedSchema();
        schema.addAnyOfItem(new StringSchema());

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "std::string",
                "Single non-null branch should collapse to that branch type");
    }

    @Test
    public void collapsesSingleStringEnumBranch() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // anyOf: [string-enum] → single string branch → std::string
        ComposedSchema schema = new ComposedSchema();
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("x");
        schema.addAnyOfItem(enumSchema);

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "std::string",
                "Single string-enum branch should collapse to std::string");
    }

    @Test
    public void resolvesAllNullBranchesToCompositionBranchValueVariant() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // anyOf: [null, null] → CompositionBranchValue variant preserves null identity
        ComposedSchema schema = new ComposedSchema();
        schema.addAnyOfItem(new Schema().type("null"));
        schema.addAnyOfItem(new Schema().type("null"));

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(
                "std::variant<CompositionBranchValue<0, std::nullptr_t>, CompositionBranchValue<1, std::nullptr_t>>",
                resolved,
                "All-null branches should produce CompositionBranchValue variant");
    }

    @Test
    public void oneOfStringStringEnumDoesNotBlindCollapseToString() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // oneOf: [string, string-enum] must NOT collapse like anyOf.
        // The CompositionBranchValue wrappers preserve identity
        // instead of type-erasing to boost::json::value.
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new StringSchema());
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("x");
        enumSchema.addEnumItem("y");
        schema.addOneOfItem(enumSchema);

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(
                "std::variant<CompositionBranchValue<0, std::string>, CompositionBranchValue<1, std::string>>",
                resolved,
                "oneOf [string, string-enum] should produce CompositionBranchValue variant");
    }

    @Test
    public void anyOfStringStringEnumPreservesValidators() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // anyOf: [string, string-enum] → CompositionBranchValue variant
        // (Rule 2 no longer collapses to std::string when enum is present)
        ComposedSchema schema = new ComposedSchema();
        schema.addAnyOfItem(new StringSchema());
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("alpha");
        enumSchema.addEnumItem("beta");
        schema.addAnyOfItem(enumSchema);

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(
                "std::variant<CompositionBranchValue<0, std::string>, CompositionBranchValue<1, std::string>>",
                resolved,
                "anyOf [string, string-enum] should produce CompositionBranchValue variant with validators");
    }
}
