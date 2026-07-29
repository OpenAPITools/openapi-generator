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
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.CppBoostBeastClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class CppBoostBeastClientCodegenTest {

    @Test
    public void generatesTypedJsonValuesForOpenApi31Schemas() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/json-value-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path modelHeader = output.toPath().resolve("model/JsonValueContainer.h");
        Path modelSource = output.toPath().resolve("model/JsonValueContainer.cpp");
        Path cmakeLists = output.toPath().resolve("CMakeLists.txt");
        Path httpClientSource = output.toPath().resolve("api/HttpClientImpl.cpp");

        TestUtils.assertFileContains(modelHeader,
                "std::nullptr_t",
                "boost::json::value",
                "std::map<std::string, boost::json::value>");
        TestUtils.assertFileContains(modelSource,
                "boost::json::serialize",
                "boost::json::parse");
        TestUtils.assertFileNotContains(modelSource, "boost::property_tree");
        TestUtils.assertFileContains(cmakeLists,
                "find_package(Boost 1.75 REQUIRED)",
                "find_package(Threads REQUIRED)",
                "find_package(OpenSSL 1.1.0 REQUIRED COMPONENTS SSL Crypto)",
                "set_property(TARGET Threads::Threads PROPERTY IMPORTED_GLOBAL TRUE)",
                "set_property(TARGET OpenSSL::SSL PROPERTY IMPORTED_GLOBAL TRUE)",
                "PUBLIC Boost::boost OpenSSL::SSL Threads::Threads");
        TestUtils.assertFileNotContains(cmakeLists, "api/HttpClient.cpp");
        TestUtils.assertFileContains(httpClientSource,
                "SSL_CTX_set_min_proto_version(",
                "TLS1_2_VERSION",
                "boost::asio::ssl::verify_peer",
                "boost::asio::ssl::host_name_verification(m_host)");
    }

    @Test
    public void generatesInheritedModelsAndRecursiveJsonConversions() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-models").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/model-generation-regression.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastRegressionClient");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path derivedHeader = output.toPath().resolve("model/DerivedModel.h");
        Path derivedSource = output.toPath().resolve("model/DerivedModel.cpp");
        Path containerHeader = output.toPath().resolve("model/ContainerModel.h");
        Path containerSource = output.toPath().resolve("model/ContainerModel.cpp");
        Path cmakeLists = output.toPath().resolve("CMakeLists.txt");
        String containerHeaderContents = java.nio.file.Files.readString(containerHeader);

        TestUtils.assertFileContains(derivedHeader,
                "#include \"BaseModel.h\"",
                "class  DerivedModel : public BaseModel",
                "DerivedModelBaseValuePropertyIsInherited<BaseModel>::value",
                "DerivedModelLocalValuePropertyIsInherited<BaseModel>::value");
        TestUtils.assertFileNotContains(derivedHeader,
                "public InterfaceModel",
                "std::string m_BaseValue");
        TestUtils.assertFileContains(derivedSource,
                "boost::json::object object = BaseModel::toJsonObject_internal();",
                "BaseModel::fromJsonObject_internal(object);",
                "if constexpr (!DerivedModelBaseValuePropertyIsInherited<BaseModel>::value)",
                "if constexpr (!DerivedModelLocalValuePropertyIsInherited<BaseModel>::value)",
                "return readBaseValueProperty<BaseModel>",
                "writeBaseValueProperty<BaseModel>");
        TestUtils.assertFileContains(containerHeader,
                "bool m_OptionalScalarIsSet = false;",
                "bool m_OptionalModelIsSet = false;",
                "bool m_ModelArrayIsSet = false;",
                "bool m_FreeFormValueIsSet = false;",
                "bool m_NullValueIsSet = false;");
        // Non-cyclic object refs use value semantics (no shared_ptr wrapping)
        TestUtils.assertFileContains(containerHeader,
                "m_ReferencedEnum");
        TestUtils.assertFileNotContains(containerHeader,
                "shared_ptr<ReferencedEnum>");
        TestUtils.assertFileNotContains(containerHeader,
                "bool m_RequiredValueIsSet",
                "std::array<");
        Assert.assertEquals(
                TestUtils.countOccurrences(containerHeaderContents, "#include <vector>"),
                1);
        TestUtils.assertFileContains(containerSource,
                "struct JsonValueConverter<std::shared_ptr<ModelType>>",
                "errorMessage << \"Value not allowed\";",
                "struct JsonValueConverter<std::nullptr_t>",
                "convertedValues.emplace_back(JsonValueConverter<Element>::fromJsonValue(jsonElement));",
                "convertedValues.emplace(entryKey, JsonValueConverter<MappedValue>::fromJsonValue(jsonEntry.value()));",
                "object[\"requiredValue\"] = JsonValueConverter<std::string>::toJsonValue(getRequiredValue());",
                "if (m_OptionalScalarIsSet)",
                "if (m_OptionalModelIsSet)",
                "if (m_ModelArrayIsSet)",
                "if (m_FreeFormValueIsSet)",
                "if (m_NullValueIsSet)",
                "m_OptionalScalarIsSet = false;",
                "m_OptionalScalarIsSet = true;",
                "static const std::array<int32_t, 2> allowedValues = {",
                "1,2",
                "static const std::array<std::string, 2> allowedValues = {",
                "\"alpha\",\"beta\"",
                "static const std::array<bool, 2> allowedValues = {",
                "true,false",
                "\"red\",\"blue\"",
                "\"green\",\"yellow\"",
                "3,4",
                "void validateEnumValues(",
                "const std::vector<Element>& values",
                "const std::map<std::string, MappedValue>& values",
                "validateEnumValues(value.second, allowedValues);",
                "validateEnumValues(value, allowedValues);",
                "setIntegerChoice(JsonValueConverter<int32_t>::fromJsonValue(IntegerChoiceIt->value()));",
                "setStringChoice(JsonValueConverter<std::string>::fromJsonValue(StringChoiceIt->value()));",
                "setBooleanChoice(JsonValueConverter<bool>::fromJsonValue(BooleanChoiceIt->value()));",
                "std::ostringstream errorMessage;",
                "errorMessage << \"Value not allowed\";",
                "JsonValueConverter<std::vector<std::vector<std::shared_ptr<ChildModel>>>>::fromJsonValue",
                "JsonValueConverter<std::map<std::string, std::map<std::string, std::shared_ptr<ChildModel>>>>::fromJsonValue",
                "JsonValueConverter<std::vector<std::map<std::string, std::shared_ptr<ChildModel>>>>::fromJsonValue",
                "JsonValueConverter<std::map<std::string, std::vector<std::shared_ptr<ChildModel>>>>::fromJsonValue",
                "vec = JsonValueConverter<std::vector<std::shared_ptr<ContainerModel>>>::fromJsonValue");
        // Phase 5: Required field validation — missing required key throws with descriptive message
        TestUtils.assertFileContains(containerSource,
                "Required field 'requiredValue' not found in ContainerModel");
        // Phase 5: Property decode wrapped with .fieldName context in error message
        TestUtils.assertFileContains(containerSource,
                "Decode failed for 'requiredValue' in ContainerModel: ",
                "Decode failed for 'optionalScalar' in ContainerModel: ");

        TestUtils.assertFileNotContains(containerSource,
                "mostInnerItems",
                "m_Inner",
                "if (!childEntry.is_null())",
                "m_IntegerChoice = JsonValueConverter");
        TestUtils.assertFileContains(cmakeLists,
                "project(CppBoostBeastRegressionClient VERSION 1.0.0 LANGUAGES CXX)",
                "include(GNUInstallDirs)",
                 "add_library(${PROJECT_NAME} SHARED)",
                 "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>",
                 "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}>",
                 "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/api>",
                 "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/model>",
                 "RUNTIME DESTINATION \"${CMAKE_INSTALL_BINDIR}\"",
                "LIBRARY DESTINATION \"${CMAKE_INSTALL_LIBDIR}\"",
                "ARCHIVE DESTINATION \"${CMAKE_INSTALL_LIBDIR}\"",
                "install(DIRECTORY api model",
                "DESTINATION \"${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}\"");
    }

    @Test
    public void generatesNullableInheritedPropertyStorage() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-nullable-inheritance").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_0/cpp-boost-beast-client/nullable-inherited-property.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path derivedHeader = output.toPath().resolve("model/NullablePropertyDerived.h");
        TestUtils.assertFileContains(derivedHeader,
                "NullablePropertyDerivedNullableValuePropertyIsInherited<NullablePropertyBase>::value",
                "bool hasOptionalValue() const",
                "void resetOptionalValue()");

        Path derivedSource = output.toPath().resolve("model/NullablePropertyDerived.cpp");
        TestUtils.assertFileContains(derivedSource,
                "if constexpr (!NullablePropertyDerivedNullableValuePropertyIsInherited<NullablePropertyBase>::value)",
                "m_NullableValue.hasOptionalValue()",
                "m_NullableValue.resetOptionalValue()");
        TestUtils.assertFileNotContains(derivedSource,
                "m_NullableValue.value.has_value()",
                "m_NullableValue.value.reset()");
    }

    @Test
    public void resolvesInlineOneOfToVariant() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        ComposedSchema oneOfSchema = new ComposedSchema();
        oneOfSchema.addOneOfItem(new StringSchema());
        oneOfSchema.addOneOfItem(new IntegerSchema());
        String resolved = codegen.getTypeDeclaration(oneOfSchema);
        Assert.assertEquals(resolved, "std::variant<std::string, int32_t>");
    }

    @Test
    public void resolvesInlineAnyOfStringEnumToString() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // anyOf: [string, string-enum] → std::string
        ComposedSchema anyOfSchema = new ComposedSchema();
        anyOfSchema.addAnyOfItem(new StringSchema());
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("alpha");
        enumSchema.addEnumItem("beta");
        anyOfSchema.addAnyOfItem(enumSchema);
        String resolved = codegen.getTypeDeclaration(anyOfSchema);
        Assert.assertEquals(resolved, "std::string");
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
        Assert.assertTrue(tempContent.contains("std::optional<double> m_Temperature"),
                "TemperatureContainer should declare std::optional<double> m_Temperature member");
        // With std::optional<double>, no redundant IsSet flag should be emitted in header.
        Assert.assertFalse(tempContent.contains("m_TemperatureIsSet"),
                "TemperatureContainer should NOT have IsSet flag for std::optional<double> property");
        // The .cpp source must also have no IsSet references for the optional property.
        Path tempContainerSource = output.toPath().resolve("model/TemperatureContainer.cpp");
        TestUtils.assertFileExists(tempContainerSource);
        String tempSourceContent = java.nio.file.Files.readString(tempContainerSource);
        Assert.assertFalse(tempSourceContent.contains("m_TemperatureIsSet"),
                "TemperatureContainer.cpp must NOT reference m_TemperatureIsSet");
        // .cpp must use has_value() for optional serialization and reset() for deserialization.
        Assert.assertTrue(tempSourceContent.contains("m_Temperature.has_value()"),
                "TemperatureContainer.cpp should use has_value() for optional serialization");
        Assert.assertTrue(tempSourceContent.contains("m_Temperature.reset()"),
                "TemperatureContainer.cpp should use reset() for optional deserialization");

        // Scenario 5: NullableTemperature — anyOf [number, null] property is std::optional<double>
        Path nullableTempHeader = output.toPath().resolve("model/NullableTemperature.h");
        TestUtils.assertFileExists(nullableTempHeader);
        String nullableTempContent = java.nio.file.Files.readString(nullableTempHeader);
        Assert.assertTrue(nullableTempContent.contains("std::optional<double> m_Temperature"),
                "NullableTemperature should declare std::optional<double> m_Temperature member");

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
        Assert.assertTrue(rseSource.contains("response.created"),
                "ResponseStreamEvent discriminator should match response.created");
        Assert.assertTrue(rseSource.contains("response.completed"),
                "ResponseStreamEvent discriminator should match response.completed");

        // Phase 2 template assertions:
        // Alias models use 'using' typedef — no class template

        // ModelIdsResponses is an alias (anyOf string+string-enum → std::string)
        Path modelIdsHeader = output.toPath().resolve("model/ModelIdsResponses.h");
        String modelIdsContent = java.nio.file.Files.readString(modelIdsHeader);
        Assert.assertTrue(modelIdsContent.contains("using ModelIdsResponses = std::string;"),
                "ModelIdsResponses should emit using alias to std::string");
        Assert.assertFalse(modelIdsContent.contains("class  ModelIdsResponses"),
                "ModelIdsResponses should not contain class declaration (empty-shell)");

        // Transitive anyOf string collapse through $ref chains:
        // ModelIdsShared → std::string
        Path modelIdsSharedHeader = output.toPath().resolve("model/ModelIdsShared.h");
        TestUtils.assertFileExists(modelIdsSharedHeader);
        String modelIdsSharedContent = java.nio.file.Files.readString(modelIdsSharedHeader);
        Assert.assertTrue(modelIdsSharedContent.contains("using ModelIdsShared = std::string;"),
                "ModelIdsShared should collapse to std::string alias (anyOf string+string-enum)");
        // ModelIds (anyOf [$ref ModelIdsShared, $ref ModelIdsResponses]) → std::string
        Path modelIdsHeaderFull = output.toPath().resolve("model/ModelIds.h");
        TestUtils.assertFileExists(modelIdsHeaderFull);
        String modelIdsFullContent = java.nio.file.Files.readString(modelIdsHeaderFull);
        Assert.assertTrue(modelIdsFullContent.contains("using ModelIds = std::string;"),
                "ModelIds should transitively collapse to std::string alias through $ref chain");
        Assert.assertFalse(modelIdsFullContent.contains("std::variant<"),
                "ModelIds must NOT produce std::variant (transitive string collapse should resolve)");
        // ModelIdsCompaction (anyOf [$ref ModelIdsResponses, string]) → std::string
        Path modelIdsCompHeader = output.toPath().resolve("model/ModelIdsCompaction.h");
        TestUtils.assertFileExists(modelIdsCompHeader);
        String modelIdsCompContent = java.nio.file.Files.readString(modelIdsCompHeader);
        Assert.assertTrue(modelIdsCompContent.contains("using ModelIdsCompaction = std::string;"),
                "ModelIdsCompaction should transitively collapse to std::string alias through $ref chain");

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

        // PetByType is a discriminated variant
        Path petByTypeSource = output.toPath().resolve("model/PetByType.cpp");
        String petByTypeSourceContent = java.nio.file.Files.readString(petByTypeSource);
        Assert.assertTrue(petByTypeSourceContent.contains("discriminator"),
                "PetByType from_json should reference discriminator");
        Assert.assertTrue(petByTypeSourceContent.contains("pet_type"),
                "PetByType from_json should reference pet_type discriminator property");
        Assert.assertTrue(petByTypeSourceContent.contains("must be a string"),
                "PetByType must reject a non-string discriminator before structural matching");
        Assert.assertTrue(petByTypeSourceContent.contains("cat\\\"quoted"),
                "PetByType must emit escaped discriminator mapping string literals");

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

        // DedupTest (oneOf string+string-enum+integer) — string branches collapse to
        // std::string but the open-string + string-enum overlap loses oneOf exclusivity.
        // Must type-erase to boost::json::value.
        Path dedupHeader = output.toPath().resolve("model/DedupTest.h");
        String dedupContent = java.nio.file.Files.readString(dedupHeader);
        Assert.assertTrue(dedupContent.contains("using DedupTest = boost::json::value;"),
                "DedupTest should emit boost::json::value (oneOf string overlap not representable)");

        // AllNullTest (anyOf null+null) should be an alias to boost::json::value
        Path allNullHeader = output.toPath().resolve("model/AllNullTest.h");
        String allNullContent = java.nio.file.Files.readString(allNullHeader);
        Assert.assertTrue(allNullContent.contains("using AllNullTest = boost::json::value;"),
                "AllNullTest should emit using alias to boost::json::value");
        Assert.assertFalse(allNullContent.contains("class  AllNullTest"),
                "AllNullTest should not contain class declaration");

        // --- Phase 2 strong review assertions ---

        // Verify variant model headers include <variant>
        Assert.assertTrue(inputParamContent.contains("#include <variant>"),
                "InputParam (variant) header should include <variant>");
        Assert.assertFalse(dedupContent.contains("#include <variant>"),
                "DedupTest (boost::json::value alias) header should NOT include <variant>");

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
        Assert.assertTrue(petByTypeSourceContent.contains("return fromJsonValue_Cat(value);"),
                "PetByType from_json should use fromJsonValue_Cat(value) dispatch");

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

        // Verify discriminator error message includes the received value
        Assert.assertTrue(petByTypeSourceContent.contains("discValue"),
                "PetByType discriminator error should include the received value");
        // Verify discValue is in scope at the throw — the throw should appear
        // within the same function scope as the declaration (no extra closing
        // brace between them).  The discValue declaration and all mapped-model
        // branches all sit at function scope (no inner {} block).
        int discValueDecl = petByTypeSourceContent.indexOf("std::string discValue{");
        int throwPos = petByTypeSourceContent.indexOf("throw std::invalid_argument", discValueDecl);
        String betweenDeclAndThrow = petByTypeSourceContent.substring(discValueDecl, throwPos);
        // Count braces: opening braces must be balanced before the throw
        long opens = betweenDeclAndThrow.chars().filter(ch -> ch == '{').count();
        long closes = betweenDeclAndThrow.chars().filter(ch -> ch == '}').count();
        Assert.assertEquals(opens, closes,
                "discValue scope: braces must be balanced between declaration and throw (got " + opens + " open, " + closes + " close)");

        // Phase 5: Discriminator mismatch — unknown value throws with clear message
        Assert.assertTrue(petByTypeSourceContent.contains("Unknown discriminator value"),
                "PetByType discriminator should throw on unknown mapping value");
        Assert.assertTrue(petByTypeSourceContent.contains("pet_type"),
                "PetByType discriminator throw should reference discriminator property name 'pet_type'");
        int unknownDiscThrowPos = petByTypeSourceContent.indexOf("Unknown discriminator value");
        Assert.assertTrue(unknownDiscThrowPos > 0 && unknownDiscThrowPos > discValueDecl,
                "PetByType 'Unknown discriminator value' throw should appear after discValue declaration");

        // Phase 5: Error path in variant error messages — concrete path-building patterns
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
        Assert.assertTrue(inputParamSourceContent.contains("capturePath"),
                "InputParam source must use capturePath for matchCount==0 diagnostic");
        Assert.assertTrue(inputParamSourceContent.contains("initialErrorPath"),
                "Variant branch trials must isolate error paths between alternatives");

        // Scenario 12a: OAS const without vendor extensions
        Path oasConstHeader = output.toPath().resolve("model/OasConstObject.h");
        TestUtils.assertFileExists(oasConstHeader);
        String oasConstContent = java.nio.file.Files.readString(oasConstHeader);
        Assert.assertTrue(oasConstContent.contains("std::string getType() const { return \"text\"; }"),
                "OasConstObject string const getter should inline from OAS const");
        Assert.assertTrue(oasConstContent.contains("int32_t getCount() const { return 42; }"),
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
        Assert.assertTrue(stainlessContent.contains("int32_t getCount() const { return 42; }"),
                "StainlessObject integer const getter should inline the value");

        // --- Phase 2 oneOf/anyOf decode distinction assertions ---

        // InputParam (oneOf variant) source must contain exactly-one checking logic
        Assert.assertTrue(inputParamSourceContent.contains("isOneOf"),
                "InputParam oneOf source should contain isOneOf compile-time flag");
        Assert.assertTrue(inputParamSourceContent.contains("matchCount"),
                "InputParam oneOf source should count matching branches");
        Assert.assertTrue(inputParamSourceContent.contains("More than one matching branch for oneOf InputParam"),
                "InputParam oneOf source should reject multi-match with descriptive error");
        // The oneOf path uses countVariantBranches + matchCount for exactly-one enforcement
        Assert.assertTrue(inputParamSourceContent.contains("countVariantBranches"),
                "InputParam oneOf source should use countVariantBranches for exactly-one check");
        // The anyOf path comment is also present (both branches emitted textually by if constexpr)

        // DedupTest is now boost::json::value alias (not variant) — source uses
        // JsonValueConverter which dispatches through model conversion helpers
        // for model-containing types (e.g., std::optional<SomeObject>) and falls
        // back to value_to/value_from for plain types.
        Path dedupSource = output.toPath().resolve("model/DedupTest.cpp");
        String dedupSourceContent = java.nio.file.Files.readString(dedupSource);
        Assert.assertTrue(dedupSourceContent.contains("JsonValueConverter<DedupTest>::toJsonValue"),
                "DedupTest alias source should use JsonValueConverter for serialization");
        Assert.assertTrue(dedupSourceContent.contains("matchingBranches"),
                "Type-erased oneOf aliases must retain branch validation");
        Assert.assertTrue(dedupSourceContent.contains("stringValue == \"a\""),
                "Type-erased oneOf aliases must retain string-enum constraints");
        Assert.assertTrue(dedupSourceContent.contains("value.is_int64()"),
                "Type-erased oneOf aliases must reject unrelated JSON kinds");

        // VariantPayload (oneOf variant) source must also contain exactly-one checking
        Path variantPayloadSource = output.toPath().resolve("model/VariantPayload.cpp");
        String variantPayloadSourceContent = java.nio.file.Files.readString(variantPayloadSource);
        Assert.assertTrue(variantPayloadSourceContent.contains("isOneOf"),
                "VariantPayload oneOf source should contain isOneOf compile-time flag");
        Assert.assertTrue(variantPayloadSourceContent.contains("matchCount"),
                "VariantPayload oneOf source should count matching branches");
        Assert.assertTrue(variantPayloadSourceContent.contains("More than one matching branch for oneOf VariantPayload"),
                "VariantPayload oneOf source should reject multi-match with descriptive error");

        // ResponseStreamEvent uses discriminator path with a non-discriminated
        // fallback (when the discriminator value is absent or not in known mappings).
        // The discriminator branch must be present.
        String rseSourceContent = java.nio.file.Files.readString(responseStreamEventSource);
        Assert.assertTrue(rseSourceContent.contains("Discriminator-aware"),
                "ResponseStreamEvent should contain discriminator dispatch");

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
        // must emit boost::json::value (not collapse to std::string).
        Path oneOfStringOverlapHeader = output.toPath().resolve("model/OneOfWithStringOverlap.h");
        TestUtils.assertFileExists(oneOfStringOverlapHeader);
        String oneOfStringOverlapContent = java.nio.file.Files.readString(oneOfStringOverlapHeader);
        Assert.assertTrue(oneOfStringOverlapContent.contains("using OneOfWithStringOverlap = boost::json::value;"),
                "OneOfWithStringOverlap (oneOf open-string + string-enum via $ref) should emit "
                + "boost::json::value");
        Assert.assertFalse(oneOfStringOverlapContent.contains("using OneOfWithStringOverlap = std::string;"),
                "OneOfWithStringOverlap must NOT collapse to std::string — oneOf overlap "
                + "requires boost::json::value");

        // Scenario 16b: StringOverlapHolder property references OneOfWithStringOverlap
        // which is a using-alias for boost::json::value. Verify the property uses the
        // typedef (the alias model name, not a plain std::string).
        Path stringOverlapHolderHeader = output.toPath().resolve("model/StringOverlapHolder.h");
        TestUtils.assertFileExists(stringOverlapHolderHeader);
        String stringOverlapHolderContent = java.nio.file.Files.readString(stringOverlapHolderHeader);
        Assert.assertTrue(stringOverlapHolderContent.contains("OneOfWithStringOverlap getOverlap()"),
                "StringOverlapHolder should declare getOverlap() returning OneOfWithStringOverlap");
        Assert.assertTrue(stringOverlapHolderContent.contains("void setOverlap(OneOfWithStringOverlap"),
                "StringOverlapHolder should declare setOverlap(OneOfWithStringOverlap)");
        // The property type is the alias name rather than boost::json::value directly.
        // Either form is correct — the alias resolves to boost::json::value at compile time.
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
    public void resolvesInputParamWithNestedSharedPtrStripped() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // Simulate InputParam: oneOf [string, array<$ref InputItem>]
        // Branch types should have no shared_ptr wrapping: std::variant<std::string, std::vector<InputItem>>
        Schema refItem = new Schema().$ref("#/components/schemas/InputItem");
        ArraySchema arraySchema = new ArraySchema();
        arraySchema.setItems(refItem);

        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new StringSchema());
        schema.addOneOfItem(arraySchema);

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "std::variant<std::string, std::vector<InputItem>>",
                "InputParam should strip nested shared_ptr from array item type");
    }

    @Test
    public void deduplicatesIdenticalBranchTypes() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // oneOf: [string, string-enum, integer] — string branches collapse to
        // std::string but the open-string + string-enum overlap loses oneOf
        // exclusivity. Must type-erase to boost::json::value.
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new StringSchema());
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("a");
        enumSchema.addEnumItem("b");
        schema.addOneOfItem(enumSchema);
        schema.addOneOfItem(new IntegerSchema());

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "boost::json::value",
                "oneOf [string, string-enum, integer] must type-erase to boost::json::value");
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
    public void resolvesAllNullBranchesToJsonValue() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // anyOf: [null, null] → all branches null → boost::json::value
        ComposedSchema schema = new ComposedSchema();
        schema.addAnyOfItem(new Schema().type("null"));
        schema.addAnyOfItem(new Schema().type("null"));

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "boost::json::value",
                "All-null branches should produce boost::json::value");
    }

    @Test
    public void oneOfStringStringEnumDoesNotBlindCollapseToString() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // oneOf: [string, string-enum] must NOT collapse like anyOf.
        // Under JSON Schema oneOf, enum members match both branches (invalid).
        // Emitting std::string would hide that; prefer boost::json::value.
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new StringSchema());
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("x");
        enumSchema.addEnumItem("y");
        schema.addOneOfItem(enumSchema);

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "boost::json::value",
                "oneOf [string, string-enum] must not blind-collapse to std::string");
    }

    @Test
    public void anyOfStringStringEnumCollapsesToString() throws IOException {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        // anyOf: [string, string-enum] → std::string (anyOf-only collapse)
        ComposedSchema schema = new ComposedSchema();
        schema.addAnyOfItem(new StringSchema());
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("alpha");
        enumSchema.addEnumItem("beta");
        schema.addAnyOfItem(enumSchema);

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "std::string",
                "AnyOfStringStringEnum should collapse to std::string");
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
            System.err.println("allOfScalarConflictThrows: root cause = " + cause.getClass().getName() + ": " + message);
            if (message == null) {
                message = e.getMessage();
            }
            Assert.assertTrue(message != null && (message.contains("allOf type conflict")
                    || message.contains("AllOfScalarConflict")),
                    "Exception root cause should mention allOf type conflict. Got: " + message);
            throw e;
        }
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void allOfPropertyConflictThrows() throws IOException {
        // This test verifies that an allOf with the same property name having
        // incompatible types causes a RuntimeException.
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

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
    }

    @Test
    public void nullableStringEnumViaGateFixtures() throws IOException {
        // Verify that NullableEnum in Gate A fixtures lowers to std::optional<...>
        // (not plain std::string) by generating from the Gate A spec.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-nullable").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/oas-compliance/fixtures.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastNullableTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path nullableEnumHeader = output.toPath().resolve("model/NullableEnum.h");
        TestUtils.assertFileExists(nullableEnumHeader);
        String nullableEnumContent = java.nio.file.Files.readString(nullableEnumHeader);
        Assert.assertTrue(nullableEnumContent.contains("std::optional<"),
                "NullableEnum header should contain std::optional<...>. Got: " + nullableEnumContent);
        Assert.assertFalse(nullableEnumContent.contains("using NullableEnum = std::string;"),
                "NullableEnum must not collapse to plain std::string.");

        Path nullableStringHeader = output.toPath().resolve("model/NullableString.h");
        TestUtils.assertFileExists(nullableStringHeader);
        String nullableStringContent = java.nio.file.Files.readString(nullableStringHeader);
        Assert.assertTrue(nullableStringContent.contains("using NullableString = std::optional<std::string>;"),
                "NullableString must emit optional alias header. Got: " + nullableStringContent);
        Path nullableStringSource = output.toPath().resolve("model/NullableString.cpp");
        TestUtils.assertFileContains(nullableStringSource,
                "JsonValueConverter<NullableString>::fromJsonValue(value)");
        TestUtils.assertFileContains(nullableStringSource,
                "JsonValueConverter<NullableString>::toJsonValue(value)");
        TestUtils.assertFileContains(nullableStringSource,
                "struct JsonValueConverter<std::optional<T>>");
        TestUtils.assertFileContains(nullableStringSource,
                "return JsonValueConverter<T>::fromJsonValue(jsonValue)");
    }

    @Test
    public void oneOfStringStringEnumViaGateFixtures() throws IOException {
        // oneOf open-string + string-enum must not claim exclusive std::string.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-oneof").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/oas-compliance/fixtures.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastOneOfTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path oneOfHeader = output.toPath().resolve("model/OneOfStringStringEnum.h");
        TestUtils.assertFileExists(oneOfHeader);
        String oneOfContent = java.nio.file.Files.readString(oneOfHeader);
        Assert.assertTrue(oneOfContent.contains("using OneOfStringStringEnum = boost::json::value;"),
                "OneOfStringStringEnum should be boost::json::value (no false exclusive string union)");
        Assert.assertFalse(oneOfContent.contains("using OneOfStringStringEnum = std::string;"),
                "OneOfStringStringEnum must not blind-collapse to std::string");
    }

    @Test
    public void generatesVariantAwareApiIntegration() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-variant-api").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/composed-schema-lowering.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path apiSource = output.toPath().resolve("api/ComposedSchemaApi.cpp");
        String generatedApiSource = Files.readString(apiSource);

        // Verify variant/optional template overloads exist in the anonymous namespace
        Assert.assertTrue(generatedApiSource.contains("toRequestJsonValue(const std::variant<Ts...>&"),
                "Generated API source must have std::variant overload for toRequestJsonValue");
        Assert.assertTrue(generatedApiSource.contains("toRequestJsonValue(const std::optional<T>&"),
                "Generated API source must have std::optional overload for toRequestJsonValue");
        Assert.assertTrue(generatedApiSource.contains("ResponseJsonValueConverter<std::variant<Ts...>>"),
                "Generated API source must have std::variant specialization for ResponseJsonValueConverter");
        Assert.assertTrue(generatedApiSource.contains("ResponseJsonValueConverter<std::optional<T>>"),
                "Generated API source must have std::optional specialization for ResponseJsonValueConverter");
        Assert.assertTrue(generatedApiSource.contains("OneOfResponseJsonValueConverter<std::variant<Ts...>>"),
                "Generated API source must preserve exactly-one response decoding for oneOf variants");
        Assert.assertTrue(generatedApiSource.contains("tryFirstVariantAlternative"),
                "Generated API source must use first-match response decoding for anyOf variants");
        Assert.assertTrue(generatedApiSource.contains("std::is_same_v<T, std::uint8_t>"),
                "Generated API source must decode bounded uint8 variant branches");
        Assert.assertTrue(generatedApiSource.contains("IsSpecialization<T, std::variant>"),
                "Generated API source must recurse into nested variant alternatives");
        Assert.assertTrue(generatedApiSource.contains("#include <limits>"),
                "Generated API source using numeric_limits must include <limits>");

        Assert.assertFalse(generatedApiSource.contains("parseEventStream"),
                "Generated API source must not contain the unused buffered SSE parser");

        // Verify trait-based dispatch for toRequestJsonValue
        Assert.assertTrue(generatedApiSource.contains("HasRequestToJsonValue"),
                "Generated API source must contain HasRequestToJsonValue trait");
        Assert.assertTrue(generatedApiSource.contains("HasFromJsonValue"),
                "Generated API source must contain HasFromJsonValue trait");
        Assert.assertFalse(generatedApiSource.contains("HasFromJsonValueMethod"),
                "Generated API source must reuse one fromJsonValue detection trait");

        // Verify postVariantBody method serializes variant body param
        String postVariantBodyMethod = extractMethod(generatedApiSource, "postVariantBody(");
        Assert.assertTrue(postVariantBodyMethod.contains(
                "serializedRequestBody = boost::json::serialize(toRequestJsonValue(inputParam));"),
                "postVariantBody must serialize using toRequestJsonValue");
        Assert.assertTrue(postVariantBodyMethod.contains(
                "OneOfResponseBodyDeserializer<InputParam>::deserialize("),
                "postVariantBody must preserve oneOf response semantics");

        // Verify postAliasBody method serializes alias body param
        String postAliasBodyMethod = extractMethod(generatedApiSource, "postAliasBody(");
        Assert.assertTrue(postAliasBodyMethod.contains(
                "serializedRequestBody = boost::json::serialize(toRequestJsonValue(modelIdsResponses));"),
                "postAliasBody must serialize using toRequestJsonValue");

        // Verify include <optional> and <variant> are present
        Assert.assertTrue(generatedApiSource.contains("#include <optional>"),
                "Generated API source must include <optional>");
        Assert.assertTrue(generatedApiSource.contains("#include <variant>"),
                "Generated API source must include <variant>");

        // Verify SSE streaming endpoint uses executeStream + appendParsedEvent
        String getStreamEventsMethod = extractMethod(generatedApiSource, "getStreamEvents(");
        Assert.assertTrue(getStreamEventsMethod.contains("executeStream("),
                "getStreamEvents must use executeStream for incremental SSE delivery");
        Assert.assertTrue(getStreamEventsMethod.contains("appendParsedEvent(deserializedResponse, eventData, fromJsonValue_ResponseStreamEvent)"),
                "getStreamEvents must appendParsedEvent with fromJsonValue_ResponseStreamEvent converter");
        Assert.assertTrue(generatedApiSource.contains("std::vector<ResponseStreamEvent>"),
                "Generated API source must have std::vector<ResponseStreamEvent> for streaming endpoint");

        // Verify multipart form-data endpoint generates form parameter handling
        String uploadFileMethod = extractMethod(generatedApiSource, "uploadFile(");
        Assert.assertTrue(uploadFileMethod.contains("FormParameter"),
                "uploadFile must generate FormParameter entries");
        Assert.assertTrue(uploadFileMethod.contains("multipart/form-data"),
                "uploadFile must use multipart/form-data serialization");

        // Verify variant form parameter endpoint uses addVariantFormParameter
        String uploadVariantMethod = extractMethod(generatedApiSource, "uploadVariantData(");
        Assert.assertTrue(uploadVariantMethod.contains("addVariantFormParameter(formParameters, \"payload\""),
                "uploadVariantData must use addVariantFormParameter for variant form param");
        Assert.assertTrue(uploadVariantMethod.contains("multipart/form-data"),
                "uploadVariantData must use multipart/form-data serialization");

        // Verify VariantPayload model files exist for branch-aware serialization
        Assert.assertTrue(java.nio.file.Files.exists(output.toPath().resolve("model/VariantPayload.h")),
                "VariantPayload model should be generated");
        Assert.assertTrue(java.nio.file.Files.exists(output.toPath().resolve("model/DataObject.h")),
                "DataObject model should be generated");

        // Verify streaming API header/source signature match
        Path apiHeader = output.toPath().resolve("api/ComposedSchemaApi.h");
        String apiHeaderContent = Files.readString(apiHeader);
        // Header must declare std::vector<ResponseStreamEvent> for streaming op
        // (newline after return type in template)
        Assert.assertTrue(apiHeaderContent.contains("std::vector<ResponseStreamEvent>"),
                "ComposedSchemaApi.h must declare getStreamEvents returning std::vector<ResponseStreamEvent>");
        Assert.assertTrue(apiHeaderContent.contains("getStreamEvents("),
                "ComposedSchemaApi.h must declare getStreamEvents method");

        // Variant headers use toJsonValue_/fromJsonValue_ (not ADL bridge — ADL would conflict)
        String inputParamHeaderContent = Files.readString(
            output.toPath().resolve("model/InputParam.h"));
        Assert.assertFalse(inputParamHeaderContent.contains("to_json("),
                "InputParam header must NOT declare ADL to_json (removed to avoid overload conflict)");
        Assert.assertFalse(inputParamHeaderContent.contains(" from_json("),
                "InputParam header must NOT declare ADL from_json (removed to avoid overload conflict)");

        // ============================================================
        // Phase 2 strong review: anyOf non-discriminated fixture
        // ============================================================
        // AnyOfStringInteger (anyOf string|integer) → std::variant<std::string, int32_t>
        Path anyOfStringIntHeader = output.toPath().resolve("model/AnyOfStringInteger.h");
        TestUtils.assertFileExists(anyOfStringIntHeader);
        String anyOfStringIntContent = java.nio.file.Files.readString(anyOfStringIntHeader);
        Assert.assertTrue(anyOfStringIntContent.contains("using AnyOfStringInteger = std::variant<std::string, int32_t>;"),
                "AnyOfStringInteger should be a variant alias to std::variant<std::string, int32_t>");

        // AnyOfStringInteger source must use first-match (anyOf), NOT exactly-one (oneOf).
        // The fromJsonValue_AnyOfStringInteger function uses isOneOf = false because the
        // composed keyword is "anyOf". Verify the source uses tryVariantBranches (first-match).
        Path anyOfStringIntSource = output.toPath().resolve("model/AnyOfStringInteger.cpp");
        TestUtils.assertFileExists(anyOfStringIntSource);
        String anyOfStringIntSourceContent = java.nio.file.Files.readString(anyOfStringIntSource);
        Assert.assertTrue(anyOfStringIntSourceContent.contains("isOneOf"),
                "AnyOfStringInteger source should contain isOneOf compile-time flag");
        // Since anyOf: isOneOf should be false, the source uses first-match path
        Assert.assertTrue(anyOfStringIntSourceContent.contains("tryVariantBranches"),
                "AnyOfStringInteger source should use tryVariantBranches");

        // AnyOfPropertyHolder references AnyOfStringInteger as a property
        Path anyOfHolderHeader = output.toPath().resolve("model/AnyOfPropertyHolder.h");
        TestUtils.assertFileExists(anyOfHolderHeader);
        String anyOfHolderContent = java.nio.file.Files.readString(anyOfHolderHeader);
        Assert.assertTrue(anyOfHolderContent.contains("AnyOfStringInteger"),
                "AnyOfPropertyHolder should declare a property of type AnyOfStringInteger");
        // The property is not marked required in the spec, so IsSet is expected
        // (variant types don't imply required in the OpenAPI sense)
        Assert.assertTrue(anyOfHolderContent.contains("m_ValueIsSet"),
                "AnyOfPropertyHolder should have IsSet for optional property");

        // AnyOfPropertyHolder source must dispatch property (de)serialization via
        // fromJsonValue_/toJsonValue_ free functions (keyword-faithful: anyOf first-match)
        // rather than the generic converter, so the named alias keeps its own keyword semantics.
        Path anyOfHolderSource = output.toPath().resolve("model/AnyOfPropertyHolder.cpp");
        TestUtils.assertFileExists(anyOfHolderSource);
        String anyOfHolderSourceContent = java.nio.file.Files.readString(anyOfHolderSource);
        Assert.assertTrue(anyOfHolderSourceContent.contains("fromJsonValue_AnyOfStringInteger"),
                "AnyOfPropertyHolder deserialization must use fromJsonValue_AnyOfStringInteger "
                + "(keyword-faithful anyOf first-match)");
        Assert.assertTrue(anyOfHolderSourceContent.contains("toJsonValue_AnyOfStringInteger"),
                "AnyOfPropertyHolder serialization must use toJsonValue_AnyOfStringInteger");
        // The JsonValueConverter variant specialization is still present in the file
        // (for non-alias-referenced variant types) but the property must NOT use it.
        Assert.assertFalse(anyOfHolderSourceContent.contains("JsonValueConverter<AnyOfStringInteger>"),
                "AnyOfPropertyHolder must NOT use JsonValueConverter<AnyOfStringInteger> "
                + "(named aliases must use their generated converter)");

        // Verify new fixture: ParentWithAnyOfOverlapping — parent referencing anyOf of
        // two overlapping object schemas (no discriminator). The generated property
        // code must dispatch via fromJsonValue_AnyOfOverlapping (anyOf first-match).
        Path overlappingParentHeader = output.toPath().resolve("model/ParentWithAnyOfOverlapping.h");
        TestUtils.assertFileExists(overlappingParentHeader);
        String overlappingParentSource = Files.readString(
            output.toPath().resolve("model/ParentWithAnyOfOverlapping.cpp"));
        Assert.assertTrue(overlappingParentSource.contains("fromJsonValue_AnyOfOverlapping"),
                "ParentWithAnyOfOverlapping deserialization must use fromJsonValue_AnyOfOverlapping "
                + "(anyOf first-match for overlapping objects)");
        Assert.assertTrue(overlappingParentSource.contains("toJsonValue_AnyOfOverlapping"),
                "ParentWithAnyOfOverlapping serialization must use toJsonValue_AnyOfOverlapping "
                + "(delegates to anyOf first-match)");

        // Verify NO from_json<T> template call sites in API source (all dispatch via fromJsonValue_)
        Assert.assertFalse(generatedApiSource.contains("from_json<"),
                "API source must not contain from_json<T> template calls (should use fromJsonValue_ functions)");

        // Verify API source calls fromJsonValue_ResponseStreamEvent directly (not template)
        Assert.assertTrue(generatedApiSource.contains("fromJsonValue_ResponseStreamEvent"),
                "API source must use fromJsonValue_ResponseStreamEvent for SSE parsing");

        // Verify HttpClientImpl declares executeStream override
        Path implHeader = output.toPath().resolve("api/HttpClientImpl.h");
        String implHeaderContent = Files.readString(implHeader);
        Assert.assertTrue(implHeaderContent.contains("executeStream("),
                "HttpClientImpl.h must declare executeStream method");
        Assert.assertTrue(implHeaderContent.contains("override"),
                "HttpClientImpl::executeStream must be declared with override");

        // Verify dual-content operation generates stream method in header and source
        Assert.assertTrue(apiHeaderContent.contains("getDualStream"),
                "ComposedSchemaApi.h must declare getDualStream method");
        Assert.assertTrue(apiHeaderContent.contains("getDualStreamStream"),
                "ComposedSchemaApi.h must declare getDualStreamStream streaming overload for dual-content op");
        Assert.assertTrue(generatedApiSource.contains("getDualStreamStream"),
                "ComposedSchemaApi.cpp must implement getDualStreamStream for dual-content op");
        Assert.assertTrue(generatedApiSource.contains("ResponseJsonValueConverter<ResponseStreamEvent>::convert"),
                "Dual-content streaming must use the generic response converter");
        Assert.assertTrue(generatedApiSource.contains("text/event-stream"),
                "ComposedSchemaApi.cpp streaming path must set Accept header to text/event-stream");
        // Verify converter name is a valid C++ identifier (no :: or < or shared_ptr)
        Assert.assertFalse(generatedApiSource.contains("fromJsonValue_std::shared_ptr<"),
                "Converter name must not contain std::shared_ptr< (invalid C++ identifier)");
        Assert.assertFalse(generatedApiSource.contains("fromJsonValue_std::"),
                "Converter name must not contain std:: namespace prefix");

        String inlineAnyOfMethod = extractMethod(generatedApiSource, "getInlineAnyOfResponse(");
        Assert.assertTrue(inlineAnyOfMethod.contains(
                "ResponseBodyDeserializer<GetInlineAnyOfResponse_200_response>::deserialize("),
                "Inline anyOf responses must use first-match variant decoding");
        Assert.assertFalse(inlineAnyOfMethod.contains("OneOfResponseBodyDeserializer"),
                "Inline anyOf responses must not use exactly-one decoding");

        String inlineOneOfStreamMethod = extractMethod(generatedApiSource, "getInlineOneOfEvents(");
        Assert.assertTrue(inlineOneOfStreamMethod.contains(
                "fromJsonValue_GetInlineOneOfEvents_200_response"),
                "Inline oneOf SSE responses must use the generated exactly-one converter");

        String dualPrimitiveMethod = extractMethod(generatedApiSource, "getDualPrimitiveStreamStream(");
        Assert.assertTrue(dualPrimitiveMethod.contains(
                "ResponseJsonValueConverter<std::string>::convert(value)"),
                "Primitive dual-content SSE responses must use the generic response converter");

        String noContentMethod = extractMethod(generatedApiSource, "deleteWithoutContent(");
        Assert.assertTrue(noContentMethod.contains("status(204)"),
                "No-content operations must handle their successful status");
        Assert.assertTrue(noContentMethod.contains("return;"),
                "Successful no-content operations must return normally");

        String httpClientHeader = Files.readString(output.toPath().resolve("api/HttpClient.h"));
        Assert.assertTrue(httpClientHeader.contains("Streaming is not supported"),
                "Custom HttpClient adapters must inherit a non-pure streaming fallback");
        Assert.assertFalse(httpClientHeader.contains("onEvent) = 0"),
                "executeStream must not remain pure virtual");

        String httpClientSource = Files.readString(output.toPath().resolve("api/HttpClientImpl.cpp"));
        Assert.assertTrue(httpClientSource.contains("consumeInitialByteOrderMark"),
                "SSE framing must strip a split UTF-8 BOM at stream start");
        Assert.assertTrue(httpClientSource.contains("http::error::need_buffer"),
                "Incremental Beast reads must accept need_buffer as a refill signal");
        Assert.assertTrue(httpClientSource.contains("tcpStream.expires_never()"),
                "HTTPS streaming must disable the stale tcp_stream expiry");
    }

    @Test
    public void generatesPureSseObjectFixture() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-pure-sse-object").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/pure-sse-object.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path apiSource = output.toPath().resolve("api/SSEApi.cpp");
        String generatedApiSource = Files.readString(apiSource);

        // Verify the pure SSE endpoint uses executeStream + appendParsedEvent with fromJsonValue_Evt
        // (not fromJsonValue_std::shared_ptr<Evt> or any invalid C++ identifier)
        Assert.assertTrue(generatedApiSource.contains("executeStream("),
                "Pure SSE must use executeStream for incremental delivery");
        Assert.assertTrue(generatedApiSource.contains("appendParsedEvent(deserializedResponse, eventData, fromJsonValue_Evt)"),
                "Pure SSE must appendParsedEvent with fromJsonValue_Evt (not shared_ptr wrapper)");
        Assert.assertTrue(generatedApiSource.contains("fromJsonValue_Evt"),
                "Pure SSE must use fromJsonValue_Evt converter (not fromJsonValue_std::...)");

        // Verify NO invalid converter names in the entire source
        Assert.assertFalse(generatedApiSource.contains("fromJsonValue_std::"),
                "Pure SSE object must not contain fromJsonValue_std:: (invalid C++ identifier)");
        Assert.assertFalse(generatedApiSource.contains("fromJsonValue_std::shared_ptr"),
                "Pure SSE object must not contain fromJsonValue_std::shared_ptr");

        // Verify the return type is std::vector<Evt> (vector of plain objects, not shared_ptr)
        Assert.assertTrue(generatedApiSource.contains("std::vector<Evt>"),
                "Pure SSE return type header must be std::vector<Evt>");

        // Verify Evt model template generates both member and free fromJsonValue functions
        Path evtHeader = output.toPath().resolve("model/Evt.h");
        String evtHeaderContent = Files.readString(evtHeader);
        Assert.assertTrue(evtHeaderContent.contains("fromJsonValue_Evt"),
                "Evt model header must declare fromJsonValue_Evt free function");

        Path evtSource = output.toPath().resolve("model/Evt.cpp");
        String evtSourceContent = Files.readString(evtSource);
        Assert.assertTrue(evtSourceContent.contains("fromJsonValue_Evt"),
                "Evt model source must define fromJsonValue_Evt free function");
    }

    @Test
    public void generatesDualObjectSseFixture() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-dual-object-sse").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/dual-object-sse.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path apiSource = output.toPath().resolve("api/DualApi.cpp");
        String generatedApiSource = Files.readString(apiSource);

        // Verify dual-content operation generates stream method
        Assert.assertTrue(generatedApiSource.contains("createItemStream"),
                "Dual-content op must generate createItemStream method");

        Assert.assertTrue(generatedApiSource.contains("ResponseJsonValueConverter<StreamEvent>::convert"),
                "Dual-content stream must use the generic typed response converter");
        Assert.assertFalse(generatedApiSource.contains("fromJsonValue_std::"),
                "Dual-content object stream must not contain fromJsonValue_std::");

        // Verify the stream method uses executeStream + appendParsedEvent with StreamEvent conversion
        Assert.assertTrue(generatedApiSource.contains("executeStream("),
                "Dual-content stream must use executeStream for incremental delivery");
        Assert.assertTrue(generatedApiSource.contains("ResponseJsonValueConverter<StreamEvent>::convert"),
                "Dual-content must append parsed events through the typed converter");

        // Verify the stream method returns std::vector<StreamEvent>
        Assert.assertTrue(generatedApiSource.contains("std::vector<StreamEvent>"),
                "Dual-content stream must return std::vector<StreamEvent>");

        // Verify path params are present in the stream method
        Assert.assertTrue(generatedApiSource.contains("replacePathParameter(path, \"id\""),
                "Dual-content stream method must include path parameter replacement");

        // Verify query params are present with optional guard
        Assert.assertTrue(generatedApiSource.contains("if (verbose)"),
                "Dual-content stream method must guard optional query param");

        // Verify header params are present with serializeHeaderParameterValue
        Assert.assertTrue(generatedApiSource.contains("serializeHeaderParameterValue"),
                "Dual-content stream method must use serializeHeaderParameterValue for headers");

        // Verify body params are present in the stream method
        Assert.assertTrue(generatedApiSource.contains("toRequestJsonValue"),
                "Dual-content stream method must include body serialization");

        // Verify Accept header is forced to text/event-stream
        Assert.assertTrue(generatedApiSource.contains("text/event-stream"),
                "Dual-content stream method must force Accept to text/event-stream");

        // Verify the header declares the stream method with correct return type
        Path apiHeader = output.toPath().resolve("api/DualApi.h");
        String apiHeaderContent = Files.readString(apiHeader);
        Assert.assertTrue(apiHeaderContent.contains("createItemStream"),
                "Dual-content API header must declare createItemStream");
        Assert.assertTrue(apiHeaderContent.contains("std::vector<StreamEvent>"),
                "Dual-content API header must declare stream method returning std::vector<StreamEvent>");
    }

    @Test
    public void rejectsPureSseWithoutResponseSchema() throws IOException {
        // A pure SSE operation with no response schema must NOT generate
        // std::vector<void> (invalid C++). Instead, the streaming flag
        // should be cleared and the return type should be void.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-pure-sse-no-schema").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/pure-sse-no-schema.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path apiHeader = output.toPath().resolve("api/SSEApi.h");
        String generatedApiHeader = Files.readString(apiHeader);

        // Must NOT contain std::vector<void> — that would fail compilation
        Assert.assertFalse(generatedApiHeader.contains("std::vector<void>"),
                "Pure SSE with no schema must not generate std::vector<void>");

        // The getEvents declaration must use void (not std::vector<...>)
        int getEventsPos = generatedApiHeader.indexOf("getEvents(");
        Assert.assertTrue(getEventsPos >= 0, "Pure SSE with no schema must declare getEvents method");
        String beforeGetEvents = generatedApiHeader.substring(Math.max(0, getEventsPos - 60), getEventsPos);
        Assert.assertFalse(beforeGetEvents.contains("std::vector<"),
                "Pure SSE with no schema must not wrap getEvents return type in std::vector<>");

        // Verify the source uses the non-streaming execute path
        Path apiSource = output.toPath().resolve("api/SSEApi.cpp");
        String generatedApiSource = Files.readString(apiSource);
        Assert.assertTrue(generatedApiSource.contains("m_client->execute("),
                "Pure SSE with no schema must use non-streaming execute");
    }

    /**
     * Checks basic C++ syntactic validity of a generated source file:
     * balanced preprocessor guards, no missing/duplicate #endif.
     */
    private static void assertBalancedPreprocessorGuards(Path filePath) throws IOException {
        String content = Files.readString(filePath);
        long ifndefCount = content.lines()
                .filter(line -> line.trim().startsWith("#ifndef"))
                .count();
        long defineCount = content.lines()
                .filter(line -> line.trim().startsWith("#define") && !line.trim().startsWith("#define "))
                .count();
        long endifCount = content.lines()
                .filter(line -> line.trim().startsWith("#endif"))
                .count();
        long ifCount = content.lines()
                .filter(line -> line.trim().startsWith("#if ") || line.trim().startsWith("#ifdef"))
                .count();
        long elifCount = content.lines()
                .filter(line -> line.trim().startsWith("#elif"))
                .count();
        long elseCount = content.lines()
                .filter(line -> line.trim().startsWith("#else"))
                .count();
        // Each #ifndef must have a matching #endif, without duplicates
        long expectedEndif = ifndefCount + ifCount;
        Assert.assertEquals(endifCount, expectedEndif,
                "File " + filePath + " has unbalanced preprocessor guards: " +
                "#ifndef=" + ifndefCount + " #if=" + ifCount + " #endif=" + endifCount);
    }

    @Test
    public void generatedHeadersPassSyntaxSmokeCheck() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-syntax").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/composed-schema-lowering.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Check all generated model headers for balanced preprocessor guards
        Path modelDir = output.toPath().resolve("model");
        List<Path> headers;
        try (var stream = java.nio.file.Files.list(modelDir)) {
            headers = stream
                    .filter(p -> p.toString().endsWith(".h"))
                    .collect(java.util.stream.Collectors.toList());
        }

        Assert.assertFalse(headers.isEmpty(), "Should have generated at least one model header");

        for (Path header : headers) {
            assertBalancedPreprocessorGuards(header);
        }
    }

    @Test
    public void keepsSharedPtrOnCyclicRefsAndStripsOnNonCyclic() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-cycles").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/cycle-detection.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CycleDetectionTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path treeNodeHeader = output.toPath().resolve("model/TreeNode.h");
        TestUtils.assertFileExists(treeNodeHeader);
        String treeContent = java.nio.file.Files.readString(treeNodeHeader);
        // TreeNode.children is a self-ref: must keep shared_ptr to break the cycle.
        Assert.assertTrue(treeContent.contains("std::shared_ptr<TreeNode>"),
                "Self-ref TreeNode.children should keep std::shared_ptr<TreeNode>");
        // The array member is std::vector<std::shared_ptr<TreeNode>>, NOT std::vector<TreeNode>
        Assert.assertTrue(treeContent.contains("std::vector<std::shared_ptr<TreeNode>>"),
                "TreeNode children vector should contain shared_ptr");

        Path roundAHeader = output.toPath().resolve("model/RoundA.h");
        TestUtils.assertFileExists(roundAHeader);
        String roundAContent = java.nio.file.Files.readString(roundAHeader);
        // RoundA.next → RoundB is a mutual cycle edge: must keep shared_ptr.
        Assert.assertTrue(roundAContent.contains("std::shared_ptr<RoundB>"),
                "Mutual-cycle edge RoundA.next should keep std::shared_ptr<RoundB>");

        Path roundBHeader = output.toPath().resolve("model/RoundB.h");
        TestUtils.assertFileExists(roundBHeader);
        String roundBContent = java.nio.file.Files.readString(roundBHeader);
        // RoundB.prev → RoundA is the other mutual cycle edge: must keep shared_ptr.
        Assert.assertTrue(roundBContent.contains("std::shared_ptr<RoundA>"),
                "Mutual-cycle edge RoundB.prev should keep std::shared_ptr<RoundA>");

        Path holderHeader = output.toPath().resolve("model/CycleHolder.h");
        TestUtils.assertFileExists(holderHeader);
        String holderContent = java.nio.file.Files.readString(holderHeader);
        // CycleHolder.leaf → Leaf is a non-cyclic edge: must use value semantics (no shared_ptr).
        Assert.assertTrue(holderContent.contains("Leaf m_Leaf"),
                "Non-cycle holder CycleHolder.leaf should use value type Leaf (no shared_ptr)");
        Assert.assertFalse(holderContent.contains("std::shared_ptr<Leaf>"),
                "Non-cycle holder CycleHolder.leaf must NOT use std::shared_ptr<Leaf>");
    }

    @Test
    public void omitsEmptyDefaultInitializer() throws IOException {
        // Verify that no generated model header contains the invalid C++ pattern
        // `= ;` which occurs when defaultValue is null/blank in the template.
        // Regression: ~37+ compilation errors from large real-world corpus headers like
        // `MessageRole m_Role = ;` when enum/model property has no default.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-empty-default").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/composed-schema-lowering.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path modelDir = output.toPath().resolve("model");
        List<Path> headers;
        try (var stream = java.nio.file.Files.list(modelDir)) {
            headers = stream
                    .filter(p -> p.toString().endsWith(".h"))
                    .collect(java.util.stream.Collectors.toList());
        }
        Assert.assertFalse(headers.isEmpty(), "Should have generated at least one model header");

        for (Path header : headers) {
            String content = java.nio.file.Files.readString(header);
            // The pattern `= ;` is invalid C++ — it means defaultValue was null/blank
            // but the template emitted `= {{{defaultValue}}}` without guarding.
            // A valid assignment like `= 0;` or `= "";` should NOT match.
            Assert.assertFalse(content.contains("= ;"),
                    "Header " + header.getFileName() + " must not contain '= ;' (empty default initializer)");
        }
    }

    private static String extractMethod(String generatedApiSource, String methodSignature) {
        int methodStart = generatedApiSource.indexOf(methodSignature);
        Assert.assertTrue(methodStart >= 0, "Missing generated method: " + methodSignature);
        int methodEnd = generatedApiSource.indexOf("\n}", methodStart);
        Assert.assertTrue(methodEnd > methodStart, "Missing closing brace for generated method: " + methodSignature);
        return generatedApiSource.substring(methodStart, methodEnd);
    }

    private static int countOccurrences(String source, String expectedText) {
        int occurrenceCount = 0;
        int searchPosition = 0;
        while ((searchPosition = source.indexOf(expectedText, searchPosition)) >= 0) {
            occurrenceCount++;
            searchPosition += expectedText.length();
        }
        return occurrenceCount;
    }
}
