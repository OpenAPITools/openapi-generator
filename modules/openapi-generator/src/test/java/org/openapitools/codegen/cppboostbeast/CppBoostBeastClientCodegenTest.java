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
                "if (!DerivedModelBaseValuePropertyIsInherited<BaseModel>::value)",
                "if (!DerivedModelLocalValuePropertyIsInherited<BaseModel>::value)",
                "return readBaseValueProperty<BaseModel>",
                "writeBaseValueProperty<BaseModel>");
        TestUtils.assertFileContains(containerHeader,
                "bool m_OptionalScalarIsSet = false;",
                "bool m_OptionalModelIsSet = false;",
                "std::shared_ptr<ReferencedEnum> m_ReferencedEnum = nullptr;",
                "bool m_ModelArrayIsSet = false;",
                "bool m_FreeFormValueIsSet = false;",
                "bool m_NullValueIsSet = false;");
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

        // Scenario 4: TemperatureContainer with nullable property
        Path tempContainerHeader = output.toPath().resolve("model/TemperatureContainer.h");
        TestUtils.assertFileExists(tempContainerHeader);
        String tempContent = java.nio.file.Files.readString(tempContainerHeader);
        Assert.assertTrue(tempContent.contains("m_Temperature"),
                "TemperatureContainer should declare m_Temperature member");
        // The nullable property type maps to std::optional<double> at the codegen level
        // (see resolvesInlineNullableToOptional). Template rendering of std::optional
        // requires the import (#include <optional>) to be wired, which is a template concern.

        // Scenario 5: OpenAITemperature — anyOf [number, null] property is std::optional<double>
        Path openaiTempHeader = output.toPath().resolve("model/OpenAITemperature.h");
        TestUtils.assertFileExists(openaiTempHeader);
        String openaiTempContent = java.nio.file.Files.readString(openaiTempHeader);
        Assert.assertTrue(openaiTempContent.contains("m_Temperature"),
                "OpenAITemperature should declare m_Temperature member");

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

        // Phase 2 template assertions:
        // Alias models use 'using' typedef — no class template

        // ModelIdsResponses is an alias (anyOf string+string-enum → std::string)
        Path modelIdsHeader = output.toPath().resolve("model/ModelIdsResponses.h");
        String modelIdsContent = java.nio.file.Files.readString(modelIdsHeader);
        Assert.assertTrue(modelIdsContent.contains("using ModelIdsResponses = std::string;"),
                "ModelIdsResponses should emit using alias to std::string");
        Assert.assertFalse(modelIdsContent.contains("class  ModelIdsResponses"),
                "ModelIdsResponses should not contain class declaration (empty-shell)");

        // InputParam is a variant (oneOf string+array → std::variant<...>)
        Path inputParamHeader = output.toPath().resolve("model/InputParam.h");
        String inputParamContent = java.nio.file.Files.readString(inputParamHeader);
        Assert.assertTrue(inputParamContent.contains("using InputParam = std::variant<std::string, std::vector<InputItem>>;"),
                "InputParam should emit using alias to std::variant");
        Assert.assertTrue(inputParamContent.contains("boost::json::value toJsonValue_InputParam(InputParam const& value);"),
                "InputParam header should declare toJsonValue_InputParam");
        Assert.assertTrue(inputParamContent.contains("InputParam fromJsonValue_InputParam(boost::json::value const& value);"),
                "InputParam header should declare fromJsonValue_InputParam");
        Assert.assertFalse(inputParamContent.contains("class  InputParam"),
                "InputParam should not contain class declaration (empty-shell)");

        // InputParam source should have to_json/from_json implementations
        Path inputParamSource = output.toPath().resolve("model/InputParam.cpp");
        String inputParamSourceContent = java.nio.file.Files.readString(inputParamSource);
        Assert.assertTrue(inputParamSourceContent.contains("toJsonValue_InputParam(InputParam const& value)"),
                "InputParam source should implement toJsonValue_InputParam");
        Assert.assertTrue(inputParamSourceContent.contains("std::visit([](auto const& v)"),
                "InputParam to_json should use std::visit");
        Assert.assertTrue(inputParamSourceContent.contains("VariantJsonHelper<"),
                "InputParam to_json should use VariantJsonHelper");

        // PetByType is a discriminated variant
        Path petByTypeSource = output.toPath().resolve("model/PetByType.cpp");
        String petByTypeSourceContent = java.nio.file.Files.readString(petByTypeSource);
        Assert.assertTrue(petByTypeSourceContent.contains("discriminator"),
                "PetByType from_json should reference discriminator");
        Assert.assertTrue(petByTypeSourceContent.contains("pet_type"),
                "PetByType from_json should reference pet_type discriminator property");

        // OptionalScore (oneOf null+number → std::optional<double>) is not generated
        // as a stand-alone file by the current pipeline — bare oneOf at component level
        // without type: object is handled differently. This will be addressed in a later phase.
        TestUtils.assertFileExists(output.toPath().resolve("model/OpenAITemperature.h"));

        // SingleBranchTest is an alias (anyOf string-enum → std::string)
        Path singleBranchHeader = output.toPath().resolve("model/SingleBranchTest.h");
        String singleBranchContent = java.nio.file.Files.readString(singleBranchHeader);
        Assert.assertTrue(singleBranchContent.contains("using SingleBranchTest = std::string;"),
                "SingleBranchTest should emit using alias to std::string");

        // DedupTest is a variant (oneOf string+string-enum+integer → std::variant<...>)
        Path dedupHeader = output.toPath().resolve("model/DedupTest.h");
        String dedupContent = java.nio.file.Files.readString(dedupHeader);
        Assert.assertTrue(dedupContent.contains("using DedupTest = std::variant<std::string, int32_t>;"),
                "DedupTest should emit using alias to std::variant");
        Assert.assertTrue(dedupContent.contains("toJsonValue_DedupTest(DedupTest const& value);"),
                "DedupTest header should declare toJsonValue_DedupTest");

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
        Assert.assertTrue(dedupContent.contains("#include <variant>"),
                "DedupTest (variant) header should include <variant>");

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
        Assert.assertTrue(petByTypeSourceContent.contains("return Cat(value);"),
                "PetByType from_json should use Cat(value) constructor");

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
        int discValueDecl = petByTypeSourceContent.indexOf("std::string discValue;");
        int throwPos = petByTypeSourceContent.indexOf("throw std::invalid_argument", discValueDecl);
        String betweenDeclAndThrow = petByTypeSourceContent.substring(discValueDecl, throwPos);
        // Count braces: opening braces must be balanced before the throw
        long opens = betweenDeclAndThrow.chars().filter(ch -> ch == '{').count();
        long closes = betweenDeclAndThrow.chars().filter(ch -> ch == '}').count();
        Assert.assertEquals(opens, closes,
                "discValue scope: braces must be balanced between declaration and throw (got " + opens + " open, " + closes + " close)");

        // Scenario 12: x-stainless-const property handling
        Path stainlessHeader = output.toPath().resolve("model/StainlessObject.h");
        TestUtils.assertFileExists(stainlessHeader);
        String stainlessContent = java.nio.file.Files.readString(stainlessHeader);
        // String const getter should return quoted value
        Assert.assertTrue(stainlessContent.contains("std::string getType() const { return \"text\"; }"),
                "StainlessObject string const getter should inline the quoted value");
        // Integer const getter should return unquoted value
        Assert.assertTrue(stainlessContent.contains("int32_t getCount() const { return 42; }"),
                "StainlessObject integer const getter should inline the value");
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

        // oneOf: [string, string-enum, integer] → after dedup: std::variant<std::string, int32_t>
        ComposedSchema schema = new ComposedSchema();
        schema.addOneOfItem(new StringSchema());
        StringSchema enumSchema = new StringSchema();
        enumSchema.addEnumItem("a");
        enumSchema.addEnumItem("b");
        schema.addOneOfItem(enumSchema);
        schema.addOneOfItem(new IntegerSchema());

        String resolved = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(resolved, "std::variant<std::string, int32_t>",
                "Duplicate string branches should be deduplicated");
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

        // Verify the SSE streaming helper is present
        Assert.assertTrue(generatedApiSource.contains("parseEventStream"),
                "Generated API source must contain parseEventStream helper");

        // Verify trait-based dispatch for toRequestJsonValue
        Assert.assertTrue(generatedApiSource.contains("HasRequestToJsonValue"),
                "Generated API source must contain HasRequestToJsonValue trait");
        Assert.assertTrue(generatedApiSource.contains("HasFromJsonValue"),
                "Generated API source must contain HasFromJsonValue trait");

        // Verify postVariantBody method serializes variant body param
        String postVariantBodyMethod = extractMethod(generatedApiSource, "postVariantBody(");
        Assert.assertTrue(postVariantBodyMethod.contains(
                "serializedRequestBody = boost::json::serialize(toRequestJsonValue(inputParam));"),
                "postVariantBody must serialize using toRequestJsonValue");
        Assert.assertTrue(postVariantBodyMethod.contains(
                "ResponseBodyDeserializer<InputParam>::deserialize("),
                "postVariantBody must deserialize using ResponseBodyDeserializer<InputParam>");

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

        // Verify SSE streaming endpoint uses parseEventStream
        String getStreamEventsMethod = extractMethod(generatedApiSource, "getStreamEvents(");
        Assert.assertTrue(getStreamEventsMethod.contains("parseEventStream<ResponseStreamEvent>(responseBody)"),
                "getStreamEvents must use parseEventStream<ResponseStreamEvent>");
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
    }

    // --- C++ compile smoke test ---

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
