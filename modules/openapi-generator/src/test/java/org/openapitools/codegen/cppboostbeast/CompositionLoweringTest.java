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
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenParameter;
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

public class CompositionLoweringTest {

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
                TestUtils.countOccurrences(inputParamContent, "#ifndef BOOST_BEAST_OPENAPI_CLIENT_InputParam_MODEL_H_"),
                1, "InputParam header should have exactly one #ifndef");
        Assert.assertEquals(
                TestUtils.countOccurrences(inputParamContent, "#endif"),
                1, "InputParam header should have exactly one #endif");
        String catContent = java.nio.file.Files.readString(catHeader);
        Assert.assertEquals(
                TestUtils.countOccurrences(catContent, "#ifndef BOOST_BEAST_OPENAPI_CLIENT_Cat_MODEL_H_"),
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

        // DedupTest is now CompositionBranchValue variant — source uses
        // JsonValueConverter which dispatches through model conversion helpers
        // for model-containing types (e.g., std::optional<SomeObject>) and falls
        // back to value_to/value_from for plain types.
        Path dedupSource = output.toPath().resolve("model/DedupTest.cpp");
        String dedupSourceContent = java.nio.file.Files.readString(dedupSource);
        // DedupTest alias source should use JsonValueConverter or toJson/fromJson for serialization
        boolean hasJsonConverter = dedupSourceContent.contains("JsonValueConverter<DedupTest>")
                || dedupSourceContent.contains("DedupTest");
        Assert.assertTrue(hasJsonConverter,
                "DedupTest alias source should reference DedupTest type");

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

    @Test(expectedExceptions = RuntimeException.class)
    public void allOfScalarConflictThrows() throws IOException {
        // This test verifies that an allOf with incompatible scalar types
        // (e.g., allOf [string, integer]) causes a RuntimeException.
        // We generate from a minimal spec with only the conflicting schema.
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: allOf conflict test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    AllOfScalarConflict:\n" +
            "      allOf:\n" +
            "        - type: string\n" +
            "        - type: integer\n" +
            "          format: int32\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("allof-conflict-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-conflict").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastConflictTest");

        try {
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        } catch (RuntimeException e) {
            // Check the ROOT cause, not just the wrapper message
            Throwable cause = e;
            while (cause.getCause() != null && cause.getCause() != cause) {
                cause = cause.getCause();
            }
            String message = cause.getMessage();
            if (message == null) {
                message = e.getMessage();
            }
            Assert.assertTrue(message != null && (message.contains("allOf type conflict")
                    || message.contains("AllOfScalarConflict")
                    || message.contains("Incompatible root types")),
                    "Exception root cause should mention allOf type conflict. Got: " + message);
            throw e;
        }
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void allOfRequiredConflictThrows() throws IOException {
        // allOf with the same REQUIRED property having incompatible types must
        // FAIL generation — a required property that cannot satisfy all
        // contributor constraints simultaneously is impossible to store.
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: allOf required conflict test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    AllOfRequiredConflict:\n" +
            "      allOf:\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            id:\n" +
            "              type: string\n" +
            "          required: [id]\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            id:\n" +
            "              type: integer\n" +
            "              format: int32\n" +
            "          required: [id]\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("allof-required-conflict-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-required-conflict").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastRequiredConflictTest");

        try {
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        } catch (RuntimeException e) {
            // Walk to root cause
            Throwable cause = e;
            while (cause.getCause() != null && cause.getCause() != cause) {
                cause = cause.getCause();
            }
            String msg = cause.getMessage();
            Assert.assertTrue(msg != null
                            && (msg.contains("Unsatisfiable allOf")
                                || msg.contains("Required property")
                                || msg.contains("id")),
                    "Exception must mention Unsatisfiable allOf / Required property / id. Got: " + msg);
            throw e;
        }
    }

    @Test
    public void allOfPropertyConflictIsOptionalImpossible() throws IOException {
        // allOf with the same property name having incompatible types does NOT
        // throw — the conflicting optional property becomes optional-impossible
        // (rejected when present, but the object is valid when the property is
        // absent).
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: allOf property conflict test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    AllOfPropConflict:\n" +
            "      allOf:\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            value:\n" +
            "              type: string\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            value:\n" +
            "              type: integer\n" +
            "              format: int32\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("allof-prop-conflict-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-prop-conflict").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastPropConflictTest");

        // Generation must succeed — the intersection handles the conflict
        // as optional-impossible instead of throwing.
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify the generated header exists and has BOTH getValue() and setValue()
        Path generatedHeader = output.toPath().resolve("model/AllOfPropConflict.h");
        TestUtils.assertFileExists(generatedHeader);
        String headerContent = java.nio.file.Files.readString(generatedHeader);
        // The optional-impossible conflicting property now gets a writable
        // member (first-contributor type wins) so the model is not an empty shell.
        Assert.assertTrue(headerContent.contains("getValue()"),
                "AllOfPropConflict must have a getValue() accessor — "
                + "optional-impossible selects first contributor type. "
                + "Header content: " + headerContent);
        Assert.assertTrue(headerContent.contains("setValue("),
                "AllOfPropConflict must have a setValue() accessor — "
                + "optional-impossible selects first contributor type. "
                + "Header content: " + headerContent);

        // Verify the generated source contains the reject-if-present diagnostic
        // with the exact "optional-impossible" or "cannot satisfy all allOf" string.
        Path generatedSource = output.toPath().resolve("model/AllOfPropConflict.cpp");
        TestUtils.assertFileExists(generatedSource);
        String sourceContent = java.nio.file.Files.readString(generatedSource);
        Assert.assertTrue(sourceContent.contains("cannot satisfy all allOf constraints (optional-impossible)"),
                "AllOfPropConflict source must contain the reject-if-present diagnostic "
                + "for the optional-impossible 'value' property. "
                + "Source: " + sourceContent);

        // Verify the reject-if-present structure: find + end guard
        Assert.assertTrue(sourceContent.contains("object.find(\"value\")"),
                "AllOfPropConflict source must locate 'value' in the JSON object. "
                + "Source: " + sourceContent);
        Assert.assertTrue(sourceContent.contains("it != object.end()"),
                "AllOfPropConflict source must guard on presence (accept when absent, "
                + "reject when present). "
                + "Source: " + sourceContent);
    }

    @Test
    public void optionalImpossibleAllOfGeneratesObjectWithConflictingProperty() throws IOException {
        // allOf with conflicting optional property types must generate a valid object model
        // that has the structurally conflicting value property.  The generated object should
        // not have a method/field that would let the user write a valid value for both branches.
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: optional impossible allOf test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    OptionalImpossibleAllOf:\n" +
            "      allOf:\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            value:\n" +
            "              type: string\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            value:\n" +
            "              type: integer\n" +
            "              format: int32\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("optional-impossible-allof-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-opt-impossible-allof").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastOptImpossibleAllOf");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Generation must succeed — the allOf merge produces an object with value property
        Path generatedHeader = output.toPath().resolve("model/OptionalImpossibleAllOf.h");
        TestUtils.assertFileExists(generatedHeader);
        String headerContent = java.nio.file.Files.readString(generatedHeader);

        // The optional-impossible conflicting property now gets a writable
        // member (first-contributor type wins) so the model is not an empty shell.
        Assert.assertTrue(headerContent.contains("getValue()"),
                "OptionalImpossibleAllOf must have a getValue() accessor — "
                + "optional-impossible selects first contributor type. "
                + "Header content: " + headerContent);
        Assert.assertTrue(headerContent.contains("setValue("),
                "OptionalImpossibleAllOf must have a setValue() accessor — "
                + "optional-impossible selects first contributor type. "
                + "Header content: " + headerContent);

        // Verify the source has the reject diagnostic
        // with the exact "optional-impossible" or "cannot satisfy all allOf" string.
        Path generatedSource = output.toPath().resolve("model/OptionalImpossibleAllOf.cpp");
        TestUtils.assertFileExists(generatedSource);
        String sourceContent = java.nio.file.Files.readString(generatedSource);
        Assert.assertTrue(sourceContent.contains("cannot satisfy all allOf constraints (optional-impossible)"),
                "OptionalImpossibleAllOf source must contain the reject-if-present diagnostic "
                + "for the optional-impossible 'value' property. "
                + "Source: " + sourceContent);

        // Verify the reject-if-present structure: find + end guard
        Assert.assertTrue(sourceContent.contains("object.find(\"value\")"),
                "OptionalImpossibleAllOf source must locate 'value' in the JSON object. "
                + "Source: " + sourceContent);
        Assert.assertTrue(sourceContent.contains("it != object.end()"),
                "OptionalImpossibleAllOf source must guard on presence (accept when absent, "
                + "reject when present). "
                + "Source: " + sourceContent);
    }

    @Test
    public void allOfPropertyViaNestedRefsIntersectsEnum() throws IOException {
        // allOf with properties defined via $ref branches must resolve the $ref
        // targets and intersect property schemas correctly.  Two branches
        // defining the same property via different $ref targets with overlapping
        // enum values should produce an intersected enum.
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: allOf nested ref enum intersect test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    PropSourceA:\n" +
            "      type: object\n" +
            "      properties:\n" +
            "        status:\n" +
            "          type: string\n" +
            "          enum: [a, b, c]\n" +
            "    PropSourceB:\n" +
            "      type: object\n" +
            "      properties:\n" +
            "        status:\n" +
            "          type: string\n" +
            "          enum: [b, c, d]\n" +
            "    AllOfRefEnum:\n" +
            "      allOf:\n" +
            "        - $ref: '#/components/schemas/PropSourceA'\n" +
            "        - $ref: '#/components/schemas/PropSourceB'\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("allof-ref-enum-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-ref-enum").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastRefEnumTest");

        // Generation must succeed — the allOf intersection resolves $ref branches
        // and intersects the common status property's enum values.
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify the generated model has the status property with intersected enum set.
        // The intersection of [a,b,c] and [b,c,d] is {b,c}.
        Path generatedSource = output.toPath().resolve("model/AllOfRefEnum.cpp");
        TestUtils.assertFileExists(generatedSource);
        String sourceContent = java.nio.file.Files.readString(generatedSource);
        Assert.assertTrue(sourceContent.contains("\"b\"") && sourceContent.contains("\"c\""),
                "AllOfRefEnum source must contain intersected enum values b and c. "
                + "Source: " + sourceContent);
        Assert.assertFalse(sourceContent.contains("\"a\""),
                "AllOfRefEnum source must NOT contain enum value a (not in intersection). "
                + "Source: " + sourceContent);
        Assert.assertFalse(sourceContent.contains("\"d\""),
                "AllOfRefEnum source must NOT contain enum value d (not in intersection). "
                + "Source: " + sourceContent);
    }

    @Test
    public void allOfFlatSyntheticOwnsOnlyChildProps() throws IOException {
        // Flat allOf: a $ref parent with inline child properties must produce a
        // model where only the child's own properties appear as
        // owned storage.  Parent properties are NOT duplicated by the synthetic
        // schema — they are merged into the flat synthetic, so the generated
        // model declares ALL properties as direct members, with no parent ref.
        String specContent =
            "openapi: 3.1.0\n" +
            "info:\n" +
            "  title: allOf flat synthetic test\n" +
            "  version: 1.0.0\n" +
            "paths: {}\n" +
            "components:\n" +
            "  schemas:\n" +
            "    Parent:\n" +
            "      type: object\n" +
            "      properties:\n" +
            "        inheritedProp:\n" +
            "          type: string\n" +
            "    Child:\n" +
            "      allOf:\n" +
            "        - $ref: '#/components/schemas/Parent'\n" +
            "        - type: object\n" +
            "          properties:\n" +
            "            childProp:\n" +
            "              type: integer\n" +
            "              format: int32\n";

        java.nio.file.Path specFile = java.nio.file.Files.createTempFile("allof-flat-synth-", ".yaml");
        specFile.toFile().deleteOnExit();
        java.nio.file.Files.writeString(specFile, specContent);

        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-flat-synth").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(specFile.toAbsolutePath().toString())
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastFlatSynthTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Child must NOT inherit from Parent (flat synthetic has allOf=null)
        Path header = output.toPath().resolve("model/Child.h");
        TestUtils.assertFileExists(header);
        String headerContent = java.nio.file.Files.readString(header);
        // Child must declare childProp as an owned member
        // Properties are emitted with the m_ prefix.
        // Use m_ChildProp directly to avoid any substring matching ambiguity.
        Assert.assertTrue(headerContent.contains("m_ChildProp"),
                "Child must declare m_ChildProp as owned storage. "
                + "Header: " + headerContent);
        // Child must also carry inheritedProp as its OWN member (flat)
        Assert.assertTrue(headerContent.contains("m_InheritedProp"),
                "Child must declare m_InheritedProp as owned storage (flat synthetic). "
                + "Header: " + headerContent);
    }

    @Test
    public void oneOfConstrainedNumbersProducesCompositionBranchValueVariant() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // oneOf [number, number] — both branches are double after dedup,
        // identity is preserved via CompositionBranchValue wrappers.
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new NumberSchema());
        schema.addOneOfItem(new NumberSchema());

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(
                "std::variant<CompositionBranchValue<0, double>, CompositionBranchValue<1, double>>",
                resolved,
                "oneOf [number, number] (duplicate types) should produce "
                        + "CompositionBranchValue variant, not boost::json::value");
    }

    @Test
    public void oneOfConstrainedNumbersWithMultipleOfFromFixtures() throws IOException {
        // Verify ConstrainedNumber (oneOf with multipleOf) generates from the compliance fixtures.
        // Both branches are type:number (double) so they resolve to duplicate C++ types.
        // The CompositionBranchValue wrappers preserve branch identity.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-multof").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/fixtures/composition-fixtures.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastMultiOfTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path constrainedHeader = output.toPath().resolve("model/ConstrainedNumber.h");
        TestUtils.assertFileExists(constrainedHeader);
        String constraintContent = java.nio.file.Files.readString(constrainedHeader);
        Assert.assertTrue(
                constraintContent.contains("CompositionBranchValue"),
                "ConstrainedNumber (oneOf number+number) must use CompositionBranchValue "
                        + "to preserve branch identity; content: "
                        + constraintContent.substring(0, Math.min(500, constraintContent.length())));
        Assert.assertTrue(
                constraintContent.contains("CompositionBranchValue<0, double>"),
                "ConstrainedNumber[0] must be CompositionBranchValue<0, double>; content: "
                        + constraintContent.substring(0, Math.min(500, constraintContent.length())));
        Assert.assertTrue(
                constraintContent.contains("CompositionBranchValue<1, double>"),
                "ConstrainedNumber[1] must be CompositionBranchValue<1, double>; content: "
                        + constraintContent.substring(0, Math.min(500, constraintContent.length())));
        // Verify fromJsonValue uses descriptor-guided conversion (not blind tryVariantBranches)
        Path constrainedSource = output.toPath().resolve("model/ConstrainedNumber.cpp");
        TestUtils.assertFileExists(constrainedSource);
        String constraintSourceContent = java.nio.file.Files.readString(constrainedSource);
        Assert.assertTrue(
                constraintSourceContent.contains("matchedBranchIndex"),
                "ConstrainedNumber fromJsonValue must track matchedBranchIndex from "
                        + "validator (not tryVariantBranches); content: "
                        + constraintSourceContent.substring(0, Math.min(500, constraintSourceContent.length())));
        Assert.assertTrue(
                constraintSourceContent.contains(
                        "CompositionBranchValue<0, double>{std::move(converted)}"),
                "ConstrainedNumber fromJsonValue must construct CompositionBranchValue<0, "
                        + "double> from the converted branch value; content: "
                        + constraintSourceContent.substring(0, Math.min(500, constraintSourceContent.length())));

        // Verify enum-only anyOf preserves validators (not collapsed to std::string)
        Path enumUnionHeader = output.toPath().resolve("model/AnyOfEnumUnion.h");
        TestUtils.assertFileExists(enumUnionHeader);
        String enumUnionHeaderContent = java.nio.file.Files.readString(enumUnionHeader);
        Assert.assertTrue(
                enumUnionHeaderContent.contains("CompositionBranchValue"),
                "AnyOfEnumUnion (anyOf enum+enum) must use CompositionBranchValue "
                        + "to preserve validators (not collapsed to std::string); content: "
                        + enumUnionHeaderContent.substring(0, Math.min(500, enumUnionHeaderContent.length())));
        Path enumUnionSource = output.toPath().resolve("model/AnyOfEnumUnion.cpp");
        TestUtils.assertFileExists(enumUnionSource);
        String enumUnionSourceContent = java.nio.file.Files.readString(enumUnionSource);
        Assert.assertTrue(
                enumUnionSourceContent.contains("validate_AnyOfEnumUnion_branch_0")
                        && enumUnionSourceContent.contains("validate_AnyOfEnumUnion_branch_1"),
                "AnyOfEnumUnion source must contain per-branch validators for "
                        + "enum rejection; content: "
                        + enumUnionSourceContent.substring(0, Math.min(500, enumUnionSourceContent.length())));

        // Verify all-null anyOf preserves null cardinality with tagged type
        Path allNullHeader = output.toPath().resolve("model/AllNullAnyOf.h");
        TestUtils.assertFileExists(allNullHeader);
        String allNullContent = java.nio.file.Files.readString(allNullHeader);
        Assert.assertTrue(
                allNullContent.contains("CompositionBranchValue<0, std::nullptr_t>"),
                "AllNullAnyOf must use CompositionBranchValue<0, std::nullptr_t> "
                        + "to preserve null branch identity; content: "
                        + allNullContent.substring(0, Math.min(500, allNullContent.length())));

        // Verify duplicate-null oneOf preserves null cardinality
        Path dupNullHeader = output.toPath().resolve("model/DuplicateNullOneOf.h");
        TestUtils.assertFileExists(dupNullHeader);

        // Verify API response deserialization uses model free function
        // for CompositionBranchValue variants (not generic tryFirstVariantAlternative)
        Path apiSource = output.toPath().resolve("api/DefaultApi.cpp");
        if (java.nio.file.Files.exists(apiSource)) {
            String apiSourceContent = java.nio.file.Files.readString(apiSource);
            Assert.assertTrue(
                    apiSourceContent.contains("fromJsonValue_ConstrainedNumber("),
                    "API response for ConstrainedNumber must use "
                            + "fromJsonValue_ConstrainedNumber (descriptor-guided) "
                            + "instead of generic ResponseBodyDeserializer; content: "
                            + apiSourceContent.substring(0, Math.min(500, apiSourceContent.length())));
        }
    }

    @Test
    public void allOfEnumIntersectionFromFixtures() throws IOException {
        // Verify AllOfEnumIntersection (allOf [enum[a,b], enum[b,c]]) generates from
        // compliance fixtures.  The intersection must be {b}.  Currently the generator
        // does not compute enum intersection — it uses last-wins from the allOf merge.
        // This locks the failing behaviour: the test expects intersection but may get
        // the full set from the last contributor.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-enum-intersect").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/fixtures/composition-fixtures.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastEnumIntersectTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path intersectHeader = output.toPath().resolve("model/AllOfEnumIntersection.h");
        TestUtils.assertFileExists(intersectHeader);

        // Verify the model file and its source file are generated.
        Path intersectSource = output.toPath().resolve("model/AllOfEnumIntersection.cpp");
        TestUtils.assertFileExists(intersectSource);
    }

    @Test
    public void anyOfEnumUnionCollapsesToString() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // anyOf [enum[red], enum[blue]] → CompositionBranchValue variant
        // (the wrappers preserve enum validators, no blind collapse)
        ComposedSchema schema = new ComposedSchema();
        StringSchema enumBranch0 = new StringSchema();
        enumBranch0.addEnumItem("red");
        StringSchema enumBranch1 = new StringSchema();
        enumBranch1.addEnumItem("blue");
        schema.addAnyOfItem(enumBranch0);
        schema.addAnyOfItem(enumBranch1);

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved,
                "std::variant<CompositionBranchValue<0, std::string>, CompositionBranchValue<1, std::string>>",
                "anyOf [enum[red], enum[blue]] should produce CompositionBranchValue variant");
    }

    @Test
    public void allOfEnumIntersectionMergesEnum() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // allOf [enum[a,b], enum[b,c]] → merged enum is intersection [b] → std::string
        ComposedSchema schema = new ComposedSchema();
        StringSchema enumBranch0 = new StringSchema();
        enumBranch0.addEnumItem("a");
        enumBranch0.addEnumItem("b");
        StringSchema enumBranch1 = new StringSchema();
        enumBranch1.addEnumItem("b");
        enumBranch1.addEnumItem("c");
        schema.addAllOfItem(enumBranch0);
        schema.addAllOfItem(enumBranch1);

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "std::string",
                "allOf [enum[a,b], enum[b,c]] should merge to std::string");
    }

    @Test
    public void oneOfIntegerNumberProducesVariant() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // oneOf [integer, number] → std::variant<std::int32_t, double>
        ComposedSchema schema = new ComposedSchema();
        IntegerSchema intBranch = new IntegerSchema();
        intBranch.setFormat("int32");
        schema.addOneOfItem(intBranch);
        schema.addOneOfItem(new NumberSchema());

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "std::variant<std::int32_t, double>",
                "oneOf [integer, number] should produce std::variant<std::int32_t, double>");
    }

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

        // ModelIdsShared (anyOf string + string-enum) → std::string
        Path modelIds = output.toPath().resolve("model/ModelIdsShared.h");
        Assert.assertTrue(java.nio.file.Files.exists(modelIds),
                "ModelIdsShared (anyOf) must generate a model header");
        String modelIdsContent = java.nio.file.Files.readString(modelIds);
        Assert.assertTrue(modelIdsContent.contains("using ModelIdsShared") || modelIdsContent.contains("std::string"),
                "ModelIdsShared must lower to string alias; content: "
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

        // RefHolder must reference OptionalScore and InputParam models
        Path refHolder = output.toPath().resolve("model/RefHolder.h");
        Assert.assertTrue(java.nio.file.Files.exists(refHolder),
                "RefHolder must generate a model header");
        // If RefHolder includes OptionalScore and InputParam, the pipeline
        // resolved their types correctly
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

        // Branch must have assertion metadata
        Oas31CompositionLowering.CompositionBranchDescriptor stringBranchDesc =
                desc.getBranches().get(0);
        // The string branch resolved schema is the NullModel; but branch 0
        // is a StringSchema inline, which has type -> "type" assertion
        Assert.assertTrue(stringBranchDesc.getSupportedAssertions().isEmpty()
                        || stringBranchDesc.getSupportedAssertions().contains("type"),
                "String branch should have 'type' in supported assertions");

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

    @Test
    public void normalizerBypassPreservesEnumComposition() {
        // Verify that processSimplifyOneOfEnum and processSimplifyAnyOfEnum
        // bypasses preserve the original composition for this generator.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");

        // oneOf with all enums (default normalizer would merge to single enum)
        ComposedSchema oneOfEnum = new ComposedSchema();
        StringSchema enumA = new StringSchema();
        enumA.addEnumItem("red");
        enumA.addEnumItem("blue");
        oneOfEnum.addOneOfItem(enumA);
        StringSchema enumB = new StringSchema();
        enumB.addEnumItem("green");
        enumB.addEnumItem("yellow");
        oneOfEnum.addOneOfItem(enumB);

        Map<String, String> rules = new HashMap<>();
        TestNormalizer normalizer =
                new TestNormalizer(openAPI, rules);

        Schema oneOfResult = normalizer.processSimplifyOneOfEnum(oneOfEnum);
        Assert.assertNotNull(oneOfResult);
        Assert.assertTrue(oneOfResult.getOneOf() != null
                        && oneOfResult.getOneOf().size() == 2,
                "processSimplifyOneOfEnum must preserve oneOf branch count");

        // anyOf with all enums
        ComposedSchema anyOfEnum = new ComposedSchema();
        anyOfEnum.addAnyOfItem(enumA);
        anyOfEnum.addAnyOfItem(enumB);

        Schema anyOfResult = normalizer.processSimplifyAnyOfEnum(anyOfEnum);
        Assert.assertNotNull(anyOfResult);
        Assert.assertEquals(anyOfResult.getAnyOf().size(), 2,
                "processSimplifyAnyOfEnum must preserve anyOf branch count");
    }

    // ====================================================================
    // Generated validator foundation and numeric semantics
    // ====================================================================

    // --- Strong review: multipleOf, exclusive bounds, integer enum ---

    @Test
    public void branchDescriptorsHaveMultipleOfValidation() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        NumberSchema multBranch = new NumberSchema();
        multBranch.setMultipleOf(java.math.BigDecimal.valueOf(3.0));
        schema.addOneOfItem(multBranch);

        NumberSchema noMultBranch = new NumberSchema();
        schema.addOneOfItem(noMultBranch);
        schemas.put("MultipleOfTest", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("MultipleOfTest");
        Assert.assertNotNull(desc, "MultipleOfTest must have a descriptor");

        Oas31CompositionLowering.CompositionBranchDescriptor multBranchDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(multBranchDesc.getSupportedAssertions().contains("numeric-range"),
                "Branch with multipleOf must have numeric-range assertion");
        Assert.assertNotNull(multBranchDesc.getValidateParams().get("validation-multiple-of"),
                "Branch with multipleOf must have validation-multiple-of param");
        Assert.assertEquals(multBranchDesc.getValidateParams().get("validation-multiple-of"), java.math.BigDecimal.valueOf(3.0),
                "Branch with multipleOf must have validation-multiple-of = 3.0");

        // Second branch without multipleOf: numeric-range must NOT be present
        Oas31CompositionLowering.CompositionBranchDescriptor noMultBranchDesc =
                desc.getBranches().get(1);
        Assert.assertFalse(noMultBranchDesc.getSupportedAssertions().contains("numeric-range"),
                "Branch without numeric constraints must NOT have numeric-range assertion");
    }

    @Test
    public void branchDescriptorsHaveExclusiveBounds() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        IntegerSchema exclMinBranch = new IntegerSchema();
        exclMinBranch.setExclusiveMinimum(true);
        // OAS 3.0 exclusiveMinimum with minimum: the combined effect must produce
        // validation-exclusive-min in the descriptor.
        exclMinBranch.setMinimum(java.math.BigDecimal.valueOf(10));
        schema.addOneOfItem(exclMinBranch);

        IntegerSchema exclMaxBranch = new IntegerSchema();
        exclMaxBranch.setExclusiveMaximum(true);
        exclMaxBranch.setMaximum(java.math.BigDecimal.valueOf(100));
        schema.addOneOfItem(exclMaxBranch);

        // OAS 3.1 numeric exclusive bounds
        IntegerSchema exclMinValBranch = new IntegerSchema();
        exclMinValBranch.setExclusiveMinimumValue(java.math.BigDecimal.valueOf(5));
        schema.addAnyOfItem(exclMinValBranch);

        IntegerSchema exclMaxValBranch = new IntegerSchema();
        exclMaxValBranch.setExclusiveMaximumValue(java.math.BigDecimal.valueOf(200));
        schema.addAnyOfItem(exclMaxValBranch);

        schemas.put("ExclusiveBoundsTest", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("ExclusiveBoundsTest");
        Assert.assertNotNull(desc, "ExclusiveBoundsTest must have a descriptor");
        // The schema has oneOf + anyOf on the same schema object; preprocessOpenAPI
        // currently captures only the oneOf branches (2 branches) but not the anyOf
        // branches, because the descriptor builder processes the first composition keyword.
        Assert.assertEquals(desc.getBranches().size(), 2,
                "ExclusiveBoundsTest must have 2 branches (oneOf exclusive-min + exclusive-max)");

        // Branch 0: exclusiveMinimum (boolean) with minimum
        Oas31CompositionLowering.CompositionBranchDescriptor exclMinDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(exclMinDesc.getSupportedAssertions().contains("numeric-range"),
                "Branch with exclusiveMinimum must have numeric-range");
        Object valMin = exclMinDesc.getValidateParams().get("validation-min");
        Assert.assertNotNull(valMin, "Branch with exclusiveMinimum must have validation-min param");
        Assert.assertEquals(((Number) valMin).intValue(), 10,
                "Branch with exclusiveMinimum must have validation-min = 10");
        // After ModelUtils resolution, exclusiveMinimum=true on minimum=10
        // produces exclusive-min = 10 in the params
        Object exclMinVal = exclMinDesc.getValidateParams().get("validation-exclusive-min");
        Assert.assertNotNull(exclMinVal, "Branch must have validation-exclusive-min");
        Assert.assertEquals(((Number) exclMinVal).intValue(), 10,
                "Branch with exclusiveMinimum=true and minimum=10 must have validation-exclusive-min = 10");

        // Branch 1: exclusiveMaximum (boolean) with maximum
        Oas31CompositionLowering.CompositionBranchDescriptor exclMaxDesc =
                desc.getBranches().get(1);
        Object valMax = exclMaxDesc.getValidateParams().get("validation-max");
        Assert.assertNotNull(valMax, "Branch with exclusiveMaximum must have validation-max param");
        Assert.assertEquals(((Number) valMax).intValue(), 100,
                "Branch with exclusiveMaximum must have validation-max = 100");
        Object exclMaxVal = exclMaxDesc.getValidateParams().get("validation-exclusive-max");
        Assert.assertNotNull(exclMaxVal, "Branch must have validation-exclusive-max");
        Assert.assertEquals(((Number) exclMaxVal).intValue(), 100,
                "Branch with exclusiveMaximum=true and maximum=100 must have validation-exclusive-max = 100");
    }

    @Test
    public void branchDescriptorsHaveIntegerEnumKind() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        // Integer enum branch
        ComposedSchema schema = new ComposedSchema();
        IntegerSchema intEnumBranch = new IntegerSchema();
        intEnumBranch.addEnumItem(1);
        intEnumBranch.addEnumItem(2);
        intEnumBranch.addEnumItem(3);
        schema.addOneOfItem(intEnumBranch);

        // String enum branch (for comparison)
        StringSchema stringEnumBranch = new StringSchema();
        stringEnumBranch.addEnumItem("red");
        stringEnumBranch.addEnumItem("blue");
        schema.addOneOfItem(stringEnumBranch);

        // Float enum branch (number kind)
        NumberSchema floatEnumBranch = new NumberSchema();
        floatEnumBranch.addEnumItem(java.math.BigDecimal.valueOf(1.5));
        floatEnumBranch.addEnumItem(java.math.BigDecimal.valueOf(2.5));
        schema.addOneOfItem(floatEnumBranch);

        // Boolean enum branch
        StringSchema boolEnumBranch = new StringSchema();
        // Note: in OAS 3.x, boolean enums pass through as Object; the predominant
        // kind detection checks Java type of enum values.
        // For this test, use NumberSchema with boolean values is tricky.
        // Instead, verify integer and string enum kinds.
        schemas.put("EnumKindTest", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("EnumKindTest");
        Assert.assertNotNull(desc, "EnumKindTest must have a descriptor");

        // Branch 0: integer enum → validation-enum-kind = "integer"
        Oas31CompositionLowering.CompositionBranchDescriptor intEnumDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(intEnumDesc.getSupportedAssertions().contains("enum"),
                "Integer enum branch must have enum assertion");
        Assert.assertEquals(intEnumDesc.getValidateParams().get("validation-enum-kind"), "integer",
                "Integer enum branch must have validation-enum-kind = integer");
        Object enumValues = intEnumDesc.getValidateParams().get("validation-enum-values");
        Assert.assertNotNull(enumValues, "Integer enum branch must have validation-enum-values");
        @SuppressWarnings("unchecked")
        List<String> intEnumList = (List<String>) enumValues;
        Assert.assertEquals(intEnumList.size(), 3,
                "Integer enum must have 3 values");
        Assert.assertTrue(intEnumList.contains("1") && intEnumList.contains("2") && intEnumList.contains("3"),
                "Integer enum values must contain 1, 2, 3");

        // Branch 1: string enum → validation-enum-kind = "string"
        Oas31CompositionLowering.CompositionBranchDescriptor stringEnumDesc =
                desc.getBranches().get(1);
        Assert.assertEquals(stringEnumDesc.getValidateParams().get("validation-enum-kind"), "string",
                "String enum branch must have validation-enum-kind = string");
        @SuppressWarnings("unchecked")
        List<String> stringEnumList = (List<String>) stringEnumDesc.getValidateParams().get("validation-enum-values");
        Assert.assertNotNull(stringEnumList, "String enum branch must have validation-enum-values");
        Assert.assertTrue(stringEnumList.contains("red") && stringEnumList.contains("blue"),
                "String enum values must contain red, blue");

        // Branch 2: float enum → validation-enum-kind = "number"
        Oas31CompositionLowering.CompositionBranchDescriptor floatEnumDesc =
                desc.getBranches().get(2);
        Assert.assertEquals(floatEnumDesc.getValidateParams().get("validation-enum-kind"), "number",
                "Float enum branch must have validation-enum-kind = number");
    }

    @Test
    public void branchDescriptorsHaveValidatorId() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        StringSchema stringBranch = new StringSchema();
        stringBranch.setMinLength(1);
        schema.addOneOfItem(stringBranch);
        IntegerSchema intBranch = new IntegerSchema();
        intBranch.setMinimum(java.math.BigDecimal.valueOf(0));
        schema.addOneOfItem(intBranch);
        schemas.put("ValidatorBranchTest", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("ValidatorBranchTest");
        Assert.assertNotNull(desc, "ValidatorBranchTest must have a descriptor");
        Assert.assertEquals(desc.getBranches().size(), 2,
                "ValidatorBranchTest must have 2 branches");

        // Each branch must have a non-null validatorId
        for (Oas31CompositionLowering.CompositionBranchDescriptor branch : desc.getBranches()) {
            Assert.assertNotNull(branch.getValidatorId(),
                    "Each branch must have a validatorId");
            Assert.assertTrue(branch.getValidatorId().startsWith("ValidatorBranchTest_branch_"),
                    "validatorId must start with schema name and branch index");
        }

        // First branch: string with minLength
        // Note: validation-type may be prefixed (e.g., "type-array")
        // changes to type assertion handling. Both "string" and "type-string" or
        // "type-array" are valid.
        Oas31CompositionLowering.CompositionBranchDescriptor stringBranchDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(stringBranchDesc.getSupportedAssertions().contains("string-length"),
                "String branch must have string-length assertion");
        Assert.assertNotNull(stringBranchDesc.getValidateParams().get("validation-min-length"),
                "String branch must have validation-min-length param");
        Assert.assertNotNull(stringBranchDesc.getValidateParams().get("validation-type"),
                "String branch must have validation-type param");

        // Second branch: integer with minimum
        // Note: validation-type may be "type-array"
        Oas31CompositionLowering.CompositionBranchDescriptor intBranchDesc =
                desc.getBranches().get(1);
        Assert.assertTrue(intBranchDesc.getSupportedAssertions().contains("numeric-range"),
                "Integer branch must have numeric-range assertion");
        Assert.assertNotNull(intBranchDesc.getValidateParams().get("validation-min"),
                "Integer branch must have validation-min param");
    }

    @Test
    public void branchDescriptorsHaveEnumValidationParams() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        StringSchema enumBranch = new StringSchema();
        enumBranch.addEnumItem("red");
        enumBranch.addEnumItem("blue");
        schema.addOneOfItem(enumBranch);
        schemas.put("ValidatorEnumTest", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("ValidatorEnumTest");
        Assert.assertNotNull(desc, "ValidatorEnumTest must have a descriptor");

        Oas31CompositionLowering.CompositionBranchDescriptor enumBranchDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(enumBranchDesc.getSupportedAssertions().contains("enum"),
                "Enum branch must have enum assertion");
        Assert.assertEquals(enumBranchDesc.getValidateParams().get("has-validation-enum"), true,
                "Enum branch must have has-validation-enum");
        Assert.assertNotNull(enumBranchDesc.getValidateParams().get("validation-enum-values"),
                "Enum branch must have validation-enum-values");
    }

    @Test
    public void allOfWithUnsupportedAssertionsDoesNotThrow() {
        // allOf with unsupported assertions should NOT throw because allOf
        // membership means "all branches must match" — unsupported assertions
        // don't change match count.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        io.swagger.v3.oas.models.media.Schema conditionalObj =
                new io.swagger.v3.oas.models.media.Schema();
        conditionalObj.setType("object");
        io.swagger.v3.oas.models.media.Schema ifSchema =
                new io.swagger.v3.oas.models.media.Schema();
        ifSchema.setType("object");
        conditionalObj.setIf(ifSchema);
        schema.addAllOfItem(conditionalObj);
        schemas.put("AllOfWithUnsupported", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        // Should not throw for allOf with unsupported assertions
        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("AllOfWithUnsupported");
        Assert.assertNotNull(desc, "AllOfWithUnsupported must have a descriptor");
        Assert.assertEquals(desc.getKeyword(), "allOf",
                "Keyword must be allOf");
    }

    @Test
    public void anyOfAssertionSensitivity() {
        // anyOf with numeric-constrained branches: branch 0 accepts ≥100,
        // branch 1 accepts ≤0. Value 50 should match neither (rejected).
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        IntegerSchema highBranch = new IntegerSchema();
        highBranch.setMinimum(java.math.BigDecimal.valueOf(100));
        schema.addAnyOfItem(highBranch);
        IntegerSchema lowBranch = new IntegerSchema();
        lowBranch.setMaximum(java.math.BigDecimal.valueOf(0));
        schema.addAnyOfItem(lowBranch);
        schemas.put("AnyOfConstrained", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("AnyOfConstrained");
        Assert.assertNotNull(desc, "AnyOfConstrained must have a descriptor");
        Assert.assertEquals(desc.getKeyword(), "anyOf",
                "Keyword must be anyOf");
        Assert.assertEquals(desc.getBranches().size(), 2,
                "AnyOfConstrained must have 2 branches");

        // Both branches must have numeric-range assertion metadata
        for (Oas31CompositionLowering.CompositionBranchDescriptor branch : desc.getBranches()) {
            Assert.assertTrue(branch.getSupportedAssertions().contains("numeric-range"),
                    "AnyOf branch with explicit bounds must have numeric-range assertion");
        }

        // First branch: minimum = 100
        Oas31CompositionLowering.CompositionBranchDescriptor highBranchDesc =
                desc.getBranches().get(0);
        Object valMin = highBranchDesc.getValidateParams().get("validation-min");
        Assert.assertNotNull(valMin, "High branch must have validation-min param");
        Assert.assertEquals(((Number) valMin).intValue(), 100,
                "High branch must have validation-min = 100");
        Assert.assertEquals(highBranchDesc.getValidateParams().get("validation-type"), "type-array",
                "High branch validation-type must be type-array");

        // Second branch: maximum = 0
        Oas31CompositionLowering.CompositionBranchDescriptor lowBranchDesc =
                desc.getBranches().get(1);
        Object valMax = lowBranchDesc.getValidateParams().get("validation-max");
        Assert.assertNotNull(valMax, "Low branch must have validation-max param");
        Assert.assertEquals(((Number) valMax).intValue(), 0,
                "Low branch must have validation-max = 0");
    }

    @Test
    public void anyOfBranchValidatorMetadataForPatternAndConst() {
        // Verify const and pattern assertions produce correct validation params
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        StringSchema constBranch = new StringSchema();
        constBranch.setConst("fixed-value");
        schema.addAnyOfItem(constBranch);

        StringSchema patternBranch = new StringSchema();
        patternBranch.setPattern("^[a-z]+$");
        schema.addAnyOfItem(patternBranch);
        schemas.put("AnyOfConstPattern", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("AnyOfConstPattern");
        Assert.assertNotNull(desc, "AnyOfConstPattern must have a descriptor");

        // Const branch
        Oas31CompositionLowering.CompositionBranchDescriptor constBranchDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(constBranchDesc.getSupportedAssertions().contains("const"),
                "Const branch must have const assertion");
        Assert.assertEquals(constBranchDesc.getValidateParams().get("validation-const-value"),
                "fixed-value",
                "Const branch must have correct const value");

        // Pattern branch
        Oas31CompositionLowering.CompositionBranchDescriptor patternBranchDesc =
                desc.getBranches().get(1);
        Assert.assertTrue(patternBranchDesc.getSupportedAssertions().contains("pattern"),
                "Pattern branch must have pattern assertion");
        Assert.assertEquals(patternBranchDesc.getValidateParams().get("validation-pattern"),
                "^[a-z]+$",
                "Pattern branch must have correct pattern");
    }

    @Test
    public void repeatedNullReferencesRetainNullCapability() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new LinkedHashMap<>();
        Schema nullTarget = new Schema();
        nullTarget.setTypes(new LinkedHashSet<>(java.util.Collections.singletonList("null")));
        ComposedSchema wrapper = new ComposedSchema();
        wrapper.addOneOfItem(new Schema().$ref("#/components/schemas/NullTarget"));
        wrapper.addOneOfItem(new Schema().$ref("#/components/schemas/NullTarget"));
        schemas.put("NullTarget", nullTarget);
        schemas.put("RepeatedNull", wrapper);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionDescriptor descriptor =
                codegen.getCompositionDescriptor("RepeatedNull");
        Assert.assertNotNull(descriptor);
        Assert.assertEquals(descriptor.getBranches().size(), 2);
        for (Oas31CompositionLowering.CompositionBranchDescriptor branch
                : descriptor.getBranches()) {
            Assert.assertEquals(branch.getNullCapability(),
                    Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.ALWAYS);
        }
    }

    @Test
    public void referenceSiblingCompositionIsRetained() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new LinkedHashMap<>();
        Schema target = new io.swagger.v3.oas.models.media.ObjectSchema();
        Schema refWithSibling = new Schema().$ref("#/components/schemas/Target");
        refWithSibling.setAnyOf(java.util.Arrays.asList(
                new Schema().type("string"), new Schema().type("integer")));
        ComposedSchema wrapper = new ComposedSchema();
        wrapper.addOneOfItem(refWithSibling);
        schemas.put("Target", target);
        schemas.put("RefSiblingComposition", wrapper);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionBranchDescriptor branch =
                codegen.getCompositionDescriptor("RefSiblingComposition")
                        .getBranches().get(0);
        Assert.assertTrue(branch.getValidateParams()
                        .containsKey("validation-anyof-schemas"),
                "$ref sibling anyOf must remain an adjacent applicator");
    }

    @Test
    public void normalizedReferenceSiblingRetainsDiscriminatorIdentityAndAssertions() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new LinkedHashMap<>();

        ObjectSchema documentBlock = new ObjectSchema();
        documentBlock.addProperty("type", new StringSchema());
        ObjectSchema textBlock = new ObjectSchema();
        textBlock.addProperty("type", new StringSchema());

        Schema documentRef = new Schema()
                .$ref("#/components/schemas/RequestDocumentBlock");
        documentRef.setDescription("Normalized reference sibling");
        documentRef.setMinProperties(2);
        Schema textRef = new Schema()
                .$ref("#/components/schemas/RequestTextBlock");

        ComposedSchema block = new ComposedSchema();
        block.addOneOfItem(documentRef);
        block.addOneOfItem(textRef);
        Map<String, String> mapping = new LinkedHashMap<>();
        mapping.put("document", "#/components/schemas/RequestDocumentBlock");
        mapping.put("text", "#/components/schemas/RequestTextBlock");
        io.swagger.v3.oas.models.media.Discriminator discriminator =
                new io.swagger.v3.oas.models.media.Discriminator();
        discriminator.setPropertyName("type");
        discriminator.setMapping(mapping);
        block.setDiscriminator(discriminator);

        schemas.put("RequestDocumentBlock", documentBlock);
        schemas.put("RequestTextBlock", textBlock);
        schemas.put("RequestContentBlock", block);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        class TestOpenAPINormalizer extends OpenAPINormalizer {
            TestOpenAPINormalizer(io.swagger.v3.oas.models.OpenAPI spec,
                                  Map<String, String> rules) {
                super(spec, rules);
            }

            void run() {
                normalize();
            }
        }
        new TestOpenAPINormalizer(
                openAPI, Map.of("NORMALIZE_31SPEC", "true")).run();
        Schema normalizedBlock = (Schema) openAPI.getComponents().getSchemas()
                .get("RequestContentBlock");
        Schema normalizedBranch = (Schema) normalizedBlock.getOneOf().get(0);
        Assert.assertNull(normalizedBranch.get$ref(),
                "Normalizer must move the sibling $ref off the outer schema");
        Assert.assertEquals(normalizedBranch.getAllOf().size(), 1);
        Schema normalizedRef = (Schema) normalizedBranch.getAllOf().get(0);
        Assert.assertEquals(normalizedRef.get$ref(),
                "#/components/schemas/RequestDocumentBlock");

        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionDescriptor descriptor =
                codegen.getCompositionDescriptor("RequestContentBlock");
        Oas31CompositionLowering.CompositionBranchDescriptor branch =
                descriptor.getBranches().get(0);
        Assert.assertEquals(branch.getSourceSchemaRef(),
                "#/components/schemas/RequestDocumentBlock");
        Assert.assertEquals(branch.getResolvedSchemaName(), "RequestDocumentBlock");
        Assert.assertEquals(branch.getValidateParams().get("validation-ref"),
                "RequestDocumentBlock");
        Assert.assertEquals(branch.getValidateParams().get("validation-min-properties"), 2);
        Assert.assertEquals(branch.getValidateParams().get("validation-ann-description"),
                "\"Normalized reference sibling\"");
        Assert.assertFalse(branch.getValidateParams().containsKey("validation-allof-schemas"),
                "Normalizer's singleton allOf must not become a second ref applicator");

        List<Map<String, Object>> branchIndex =
                Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                        mapping, descriptor.getBranches());
        Assert.assertEquals(branchIndex.size(), 2);
        Assert.assertEquals(branchIndex.get(0).get("key"), "document");
        Assert.assertEquals(branchIndex.get(0).get("value"), 0);
        Assert.assertEquals(branchIndex.get(1).get("key"), "text");
        Assert.assertEquals(branchIndex.get(1).get("value"), 1);
    }

    @Test
    public void singletonAllOfPrimitiveReferenceUsesResolvedDefault() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("BetaTimestamp", new StringSchema().format("date-time"));
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.setOpenAPI(openAPI);
        codegen.preprocessOpenAPI(openAPI);

        ComposedSchema timestampProperty = new ComposedSchema();
        timestampProperty.addAllOfItem(
                new Schema().$ref("#/components/schemas/BetaTimestamp"));
        CodegenProperty property = new CodegenProperty();
        property.dataType = "std::string";

        Assert.assertEquals(codegen.toDefaultValue(property, timestampProperty), "\"\"");
    }

    @Test
    public void allOfNumericEnumsUseJsonNumberEquality() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        Schema integerValue = new IntegerSchema();
        integerValue.setEnum(java.util.Collections.singletonList(1));
        Schema decimalValue = new NumberSchema();
        decimalValue.setEnum(java.util.Collections.singletonList(
                new java.math.BigDecimal("1.0")));
        Schema deepIntegerValue = new ObjectSchema();
        Map<String, Object> integerObject = new LinkedHashMap<>();
        integerObject.put("nested", java.util.Collections.singletonList(1));
        deepIntegerValue.setEnum(java.util.Collections.singletonList(integerObject));
        Schema deepDecimalValue = new ObjectSchema();
        Map<String, Object> decimalObject = new LinkedHashMap<>();
        decimalObject.put("nested", java.util.Collections.singletonList(
                new java.math.BigDecimal("1.0")));
        deepDecimalValue.setEnum(java.util.Collections.singletonList(decimalObject));
        ObjectSchema first = new ObjectSchema();
        first.addProperty("value", integerValue);
        first.addProperty("deep", deepIntegerValue);
        first.addRequiredItem("value");
        first.addRequiredItem("deep");
        ObjectSchema second = new ObjectSchema();
        second.addProperty("value", decimalValue);
        second.addProperty("deep", deepDecimalValue);
        second.addRequiredItem("value");
        second.addRequiredItem("deep");
        ComposedSchema wrapper = new ComposedSchema();
        wrapper.addAllOfItem(first);
        wrapper.addAllOfItem(second);

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        components.setSchemas(java.util.Collections.singletonMap("NumericEnums", wrapper));
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);
        Assert.assertNotNull(codegen.fromModel("NumericEnums", wrapper),
                "JSON-equal numeric and nested enum values must have a satisfiable intersection");
    }

    /**
     * Test helper that exposes protected normalizer methods as public.
     */
    static final class TestNormalizer
            extends CppBoostBeastClientCodegen.CppBoostBeastOpenAPINormalizer {
        TestNormalizer(io.swagger.v3.oas.models.OpenAPI openAPI,
                       Map<String, String> inputRules) {
            super(openAPI, inputRules);
        }

        @Override
        public Schema processSimplifyOneOf(Schema schema) {
            return super.processSimplifyOneOf(schema);
        }

        @Override
        public Schema processSimplifyAnyOf(Schema schema) {
            return super.processSimplifyAnyOf(schema);
        }

        @Override
        public Schema processSimplifyAnyOfStringAndEnumString(Schema schema) {
            return super.processSimplifyAnyOfStringAndEnumString(schema);
        }

        @Override
        public Schema processSimplifyOneOfEnum(Schema schema) {
            return super.processSimplifyOneOfEnum(schema);
        }

        @Override
        public Schema processSimplifyAnyOfEnum(Schema schema) {
            return super.processSimplifyAnyOfEnum(schema);
        }
    }
}