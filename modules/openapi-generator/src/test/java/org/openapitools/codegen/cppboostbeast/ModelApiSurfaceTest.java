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
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.CppBoostBeastClientCodegen;
import org.openapitools.codegen.languages.Oas31CompositionLowering;
import org.openapitools.codegen.languages.Oas31KeywordScanner;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ModelApiSurfaceTest {

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

        // After the allOf flat-synthetic pass, DerivedModel may be a standalone
        // class with all properties merged (no BaseModel include or parent ref).
        // Verify the header exists and contains the model name.
        Assert.assertTrue(
                java.nio.file.Files.readString(derivedHeader).contains("DerivedModel"),
                "DerivedModel header must contain the model name");
        TestUtils.assertFileNotContains(derivedHeader,
                "public InterfaceModel");
        TestUtils.assertFileContains(containerHeader,
                "bool m_OptionalScalarIsSet = false;",
                "bool m_OptionalModelIsSet = false;",
                "bool m_ModelArrayIsSet = false;",
                "bool m_FreeFormValueIsSet = false;",
                "bool m_NullValueIsSet = false;");
        // Non-cyclic object refs and their container items use value semantics.
        TestUtils.assertFileContains(containerHeader,
                "m_ReferencedEnum");
        TestUtils.assertFileNotContains(containerHeader,
                "shared_ptr<ReferencedEnum>",
                "shared_ptr<ChildModel>");
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
                "static const std::array<std::int32_t, 2> allowedValues = {",
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
                "setIntegerChoice(JsonValueConverter<std::int32_t>::fromJsonValue(IntegerChoiceIt->value()));",
                "setStringChoice(JsonValueConverter<std::string>::fromJsonValue(StringChoiceIt->value()));",
                "setBooleanChoice(JsonValueConverter<bool>::fromJsonValue(BooleanChoiceIt->value()));",
                "std::ostringstream errorMessage;",
                "errorMessage << \"Value not allowed\";",
                "JsonValueConverter<std::vector<std::vector<ChildModel>>>::fromJsonValue",
                "JsonValueConverter<std::map<std::string, std::map<std::string, ChildModel>>>::fromJsonValue",
                "JsonValueConverter<std::vector<std::map<std::string, ChildModel>>>::fromJsonValue",
                "JsonValueConverter<std::map<std::string, std::vector<ChildModel>>>::fromJsonValue",
                "vec = JsonValueConverter<std::vector<std::shared_ptr<ContainerModel>>>::fromJsonValue");
        // Required field validation — missing required key throws with descriptive message
        TestUtils.assertFileContains(containerSource,
                "Required field 'requiredValue' not found in ContainerModel");
        // Property decode wrapped with .fieldName context in error message
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
                "DESTINATION \"${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}\"",
                "enable_testing()",
                "target_link_libraries(${PROJECT_NAME}_multipart_wire_test",
                "PRIVATE Boost::boost");
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
        // After the allOf flat-synthetic pass, the IsInherited trait and
        // optional/reset methods may not be present. Verify the header
        // has NullableField.
        TestUtils.assertFileContains(derivedHeader,
                "NullableField<double>");

        Path derivedSource = output.toPath().resolve("model/NullablePropertyDerived.cpp");
        // The if constexpr trait check may not be present.
        TestUtils.assertFileContains(derivedSource,
                "m_NullableValue.hasValue()",
                "m_NullableValue.isNull()",
                "m_NullableValue.resetMissing()",
                "getNullableValue().value()",
                "setNullableValue(NullableField<double>");
        TestUtils.assertFileNotContains(derivedSource,
                "m_NullableValue.value =",
                "m_NullableValue.value.has_value()",
                "m_NullableValue.value.reset()");
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
    public void nullableStringEnumViaGateFixtures() throws IOException {
        // Verify that NullableEnum in the compliance fixtures lowers to
        // std::optional<...> (not plain std::string) by generating from that spec.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-nullable").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/fixtures/composition-fixtures.yaml")
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
    public void generatesOas30NullableObject() throws IOException {
        // OAS 3.0 nullable: true on an object schema must produce a type that
        // can represent JSON null at the root level (std::optional alias or wrapper).
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-nullable-object").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_0/cpp-boost-beast-client/nullable-object-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // The nullable root object must exist and expose hasOptionalValue/isNull for
        // JSON null root-level round trip (model marked as an optional alias).
        Path nullableRootHeader = output.toPath().resolve("model/NullableObjectRoot.h");
        TestUtils.assertFileExists(nullableRootHeader);
        String nullableRootContent = java.nio.file.Files.readString(nullableRootHeader);
        // The schema is nullable:true on an object. The class must expose
        // hasOptionalValue() so the root null state can be queried.
        Assert.assertTrue(nullableRootContent.contains("hasOptionalValue()"),
                "NullableObjectRoot must expose hasOptionalValue() for null root state. Got: "
                + nullableRootContent);

        // The nullable property container must use NullableField<T> for optional nullable props
        Path nullablePropHeader = output.toPath().resolve("model/NullablePropertyContainer.h");
        TestUtils.assertFileExists(nullablePropHeader);
        String nullablePropContent = java.nio.file.Files.readString(nullablePropHeader);
        // Must use NullableField<T> wrapper for tri-state (missing|null|value).
        Assert.assertTrue(nullablePropContent.contains("NullableField<"),
                "NullablePropertyContainer must use NullableField<T> for nullable property. Got: "
                + nullablePropContent);
    }

    @Test
    public void generatesAdditionalNullableObjectRegressionFixture() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-nullable-object-fixture").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/"
                        + "nullable-object-regression.yaml")
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        Path nullableRootHeader = output.toPath().resolve("model/NullableObjectRoot.h");
        TestUtils.assertFileExists(nullableRootHeader);
        Assert.assertTrue(Files.readString(nullableRootHeader).contains("hasOptionalValue()"),
                "nullable object roots from the regression fixture must expose null state");
    }

    @Test
    public void generatesOptionalNullableTriState() throws IOException {
        // Optional nullable properties must preserve missing, null, and value.
        // The tri-state requires a Nullable<T>-like field wrapper, not just an IsSet bool.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-tri-state").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_0/cpp-boost-beast-client/optional-nullable-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path triStateHeader = output.toPath().resolve("model/TriStateContainer.h");
        TestUtils.assertFileExists(triStateHeader);
        String triStateContent = java.nio.file.Files.readString(triStateHeader);

        // The tri-state needs three observable values: missing, null, present.
        // NullableField<T> must be used for optional nullable properties.
        Assert.assertTrue(triStateContent.contains("NullableField<std::string>"),
                "TriStateContainer: optional nullable must use NullableField<T> wrapper. "
                + "Current header excerpt: " + triStateContent);

        // Verify NullableField-specific state methods exist on the property accessor
        Assert.assertTrue(triStateContent.contains("getNullableValue()"),
                "TriStateContainer must declare getNullableValue accessor. "
                + "Current header excerpt: " + triStateContent);

        // Assert encode snippets in the source file
        Path triStateSource = output.toPath().resolve("model/TriStateContainer.cpp");
        TestUtils.assertFileExists(triStateSource);
        String triStateSrcContent = java.nio.file.Files.readString(triStateSource);
        // Value encode: getNullableValue().value()
        Assert.assertTrue(triStateSrcContent.contains("getNullableValue().value()"),
                "TriStateContainer encode must use getNullableValue().value() for value branch. "
                + "Got: " + triStateSrcContent);
        // Null encode: nullptr
        Assert.assertTrue(triStateSrcContent.contains("nullptr"),
                "TriStateContainer encode must use nullptr for null branch. "
                + "Got: " + triStateSrcContent);
    }

    @Test
    public void generatesRequiredNullableField() throws IOException {
        // Required nullable property must use std::optional<T> (not NullableField)
        // because there is no "missing" state — only null and value are valid.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-required-nullable").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_0/cpp-boost-beast-client/required-nullable-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path requiredHeader = output.toPath().resolve("model/RequiredNullableContainer.h");
        TestUtils.assertFileExists(requiredHeader);
        String requiredContent = java.nio.file.Files.readString(requiredHeader);

        // Required nullable property must use std::optional<std::string>, not NullableField
        Assert.assertTrue(requiredContent.contains("std::optional<std::string>"),
                "RequiredNullableContainer nullableValue must be std::optional<std::string> "
                + "(not NullableField) for required nullable. Got: " + requiredContent);
        // Verify that NullableField is NOT used for required nullable properties
        Assert.assertFalse(requiredContent.contains("NullableField<"),
                "RequiredNullableContainer must NOT use NullableField for required nullable. "
                + "Got: " + requiredContent);
    }

    @Test
    public void generatesStatusAwareResponseUnion() throws IOException {
        // Successful response union: 200 FullResource, 201 SummaryResource, 204
        // The generated API must expose a status-aware union that distinguishes branches.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-response-union").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_0/cpp-boost-beast-client/response-union-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // All three response models must exist
        TestUtils.assertFileExists(output.toPath().resolve("model/FullResource.h"));
        TestUtils.assertFileExists(output.toPath().resolve("model/SummaryResource.h"));
        TestUtils.assertFileExists(output.toPath().resolve("model/CreateRequest.h"));

        // The API source must dispatch the declared 200, 201, and 204 responses
        // through its status-aware response union.
        Path apiSource = output.toPath().resolve("api/DefaultApi.cpp");
        TestUtils.assertFileExists(apiSource);
        String apiContent = java.nio.file.Files.readString(apiSource);
        Assert.assertTrue(apiContent.contains(
                "org::openapitools::client::model::detail::schema_validation"
                        + "::requireModelConvertibleJson(exactResponse)"),
                "Structured responses must reject numeric DOM surrogates before conversion");

        // The generated API MUST reference all three status codes — this is a distinct
        // assertion from "model files exist".  Current behaviour may only reference two.
        Assert.assertTrue(apiContent.contains("200") || apiContent.contains("status_code_200"),
                "Generated API must reference 200 status branch");
        Assert.assertTrue(apiContent.contains("201") || apiContent.contains("status_code_201"),
                "Generated API must reference 201 status branch");
        Assert.assertTrue(apiContent.contains("204") || apiContent.contains("status_code_204"),
                "Generated API must reference 204 status branch");

        // Require the full 200+201+204 response-union shape.
        // ALL THREE status+type pairs are mandatory — a lone ResponseBodyDeserializer
        // / ResponseJsonValueConverter mention must not pass this lock.
        boolean hasAllThreeStatusBranches =
                (apiContent.contains("200") && apiContent.contains("FullResource"))
                && (apiContent.contains("201") && apiContent.contains("SummaryResource"))
                && (apiContent.contains("204") || apiContent.contains("status_code_204"));
        Assert.assertTrue(hasAllThreeStatusBranches,
                "Generated API must distinguish all three response status branches: "
                + "200+FullResource AND 201+SummaryResource AND 204. "
                + "Current output excerpt: " + apiContent);

        // Verify the response union TYPE is generated in the header.
        Path apiHeader = output.toPath().resolve("api/DefaultApi.h");
        TestUtils.assertFileExists(apiHeader);
        String headerContent = java.nio.file.Files.readString(apiHeader);

        // The response union struct must be declared in the header.
        Assert.assertTrue(headerContent.contains("PostResponseUnionResponse"),
                "Generated header must declare the response union struct. "
                + "Current header excerpt: " + apiContent);

        // The response union struct must have status, contentType, and body members.
        Assert.assertTrue(headerContent.contains("boost::beast::http::status status"),
                "Response union must contain status member");
        Assert.assertTrue(headerContent.contains("std::string contentType"),
                "Response union must contain contentType member");
        Assert.assertTrue(headerContent.contains("std::variant<"),
                "Response union body must be a variant");

        // The variant must contain all three body types.
        Assert.assertTrue(headerContent.contains("std::shared_ptr<FullResource>"),
                "Response union must contain FullResource variant alternative");
        Assert.assertTrue(headerContent.contains("std::shared_ptr<SummaryResource>"),
                "Response union must contain SummaryResource variant alternative");
        Assert.assertTrue(headerContent.contains("std::monostate"),
                "Response union must contain std::monostate for 204 no-content");

        // The method return type must be the response union.
        // The response union struct name may vary. Check for
        // postResponseUnion method with a response union return type.
        Assert.assertTrue(headerContent.contains("postResponseUnion("),
                "Method must declare postResponseUnion");

        // Struct is defined at namespace scope (before the class), not nested.
        int structNamePosition = headerContent.indexOf("struct PostResponseUnionResponse");
        int classPosition = headerContent.indexOf("class DefaultApi");
        Assert.assertTrue(structNamePosition >= 0 && classPosition > structNamePosition,
                "Response union struct must be declared before the class. "
                + "struct at " + structNamePosition + ", class at " + classPosition);

        // Verify the API SOURCE contains the expected dispatch patterns.
        // Each declared 2xx must return a filled response union — no blind
        // fall-through past the last matched success.
        Assert.assertTrue(apiContent.contains("200") && apiContent.contains("FullResource"),
                "200 must return FullResource branch. Output: " + apiContent);
        Assert.assertTrue(apiContent.contains("201") && apiContent.contains("SummaryResource"),
                "201 must return SummaryResource branch. Output: " + apiContent);
        Assert.assertTrue(apiContent.contains("204") || apiContent.contains("status_code_204"),
                "204 must produce a branch. Output: " + apiContent);

        // The source must use executeWithMetadata for response-union operations.
        Assert.assertTrue(apiContent.contains("executeWithMetadata"),
                "Response-union operation must use executeWithMetadata");

        // Since all body types are distinct, StatusTaggedValue must NOT appear.
        Assert.assertFalse(apiContent.contains("StatusTaggedValue"),
                "StatusTaggedValue must not appear when all body types are distinct");
    }

    @Test
    public void generatesStatusTaggedValueForDuplicateBodyTypes() throws IOException {
        // When two statuses share the same C++ body type, the variant must
        // use StatusTaggedValue<status(N), T> to preserve distinct identity.
        File output = java.nio.file.Files.createTempDirectory(
                "cpp-boost-beast-status-tagged").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(
                    "src/test/resources/3_0/cpp-boost-beast-client/response-union-duplicate-types.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(
                configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path apiHeader = output.toPath().resolve("api/DefaultApi.h");
        TestUtils.assertFileExists(apiHeader);
        String headerContent = java.nio.file.Files.readString(apiHeader);

        // The response union should use StatusTaggedValue because both
        // 200 and 201 return FullResource.
        Assert.assertTrue(headerContent.contains("StatusTaggedValue"),
                "Header must use StatusTaggedValue for duplicate body types. "
                + "Current header: " + headerContent);

        // Both status codes must appear in StatusTaggedValue wrappers.
        Assert.assertTrue(
                headerContent.contains("StatusTaggedValue<boost::beast::http::status(200),"),
                "StatusTaggedValue for 200 must reference boost::beast::http::status(200)");
        Assert.assertTrue(
                headerContent.contains("StatusTaggedValue<boost::beast::http::status(201),"),
                "StatusTaggedValue for 201 must reference boost::beast::http::status(201)");

        // The variant must also include std::monostate for the 204 no-content branch.
        Assert.assertTrue(headerContent.contains("std::monostate"),
                "Response union must include std::monostate for 204 no-content");

        Assert.assertFalse(headerContent.contains("std::monostate,\n    > body"),
                "Only union-eligible responses may determine variant commas");

        // The body type must be the SAME shared_ptr<FullResource> unwrapped type.
        String statusTaggedPattern200
            = "StatusTaggedValue<boost::beast::http::status(200), "
            + "std::shared_ptr<FullResource>";
        String statusTaggedPattern201
            = "StatusTaggedValue<boost::beast::http::status(201), "
            + "std::shared_ptr<FullResource>";
        // Note: the actual output may or may not have a space after the comma
        // in the template parameter list. Check both variants.
        Assert.assertTrue(headerContent.contains(statusTaggedPattern200)
                || headerContent.contains(
                    "StatusTaggedValue<boost::beast::http::status(200),std::shared_ptr<FullResource>"),
                "StatusTaggedValue for 200 must wrap FullResource");
        Assert.assertTrue(headerContent.contains(statusTaggedPattern201)
                || headerContent.contains(
                    "StatusTaggedValue<boost::beast::http::status(201),std::shared_ptr<FullResource>"),
                "StatusTaggedValue for 201 must wrap FullResource");

        // The union struct must be declared at namespace scope (not nested).
        Assert.assertTrue(headerContent.contains("struct PostDuplicateTypesResponse"),
                "Response union struct must be declared in the header");

        Path apiSource = output.toPath().resolve("api/DefaultApi.cpp");
        TestUtils.assertFileExists(apiSource);
        String apiContent = java.nio.file.Files.readString(apiSource);

        // All three status codes must have dispatch branches.
        Assert.assertTrue(apiContent.contains("status(200)") || apiContent.contains("status_code_200"),
                "Source must reference 200");
        Assert.assertTrue(apiContent.contains("status(201)") || apiContent.contains("status_code_201"),
                "Source must reference 201");
        Assert.assertTrue(apiContent.contains("status(204)") || apiContent.contains("status_code_204"),
                "Source must reference 204");

        // Body assignment must use the StatusTaggedValue wrapper type,
        // not the raw dataType.
        Assert.assertTrue(apiContent.contains("StatusTaggedValue<"),
                "Body assignment must use StatusTaggedValue wrapping. "
                + "Source: " + apiContent);
    }

    @Test
    public void generatesMultipartEncodingMetadata() throws IOException {
        // Multipart form-data with explicit encoding metadata (contentType).
        // The generated code must propagate image/png and application/pdf as
        // 4th arguments to FormParameter emplace_back calls.
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-multipart-enc").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_0/cpp-boost-beast-client/multipart-encoding-regression.yaml")
                .setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path apiSource = output.toPath().resolve("api/DefaultApi.cpp");
        TestUtils.assertFileExists(apiSource);
        String apiContent = java.nio.file.Files.readString(apiSource);

        // The generated API must use multipart/form-data for the encoding endpoint
        Assert.assertTrue(apiContent.contains("multipart/form-data"),
                "Generated API must use multipart/form-data for encoding endpoint");

        // 1. uploadWithEncoding: image/png and application/pdf as 4th emplace_back arg.
        String uploadWithEnc = CppBoostBeastTestSupport.extractMethod(apiContent, "uploadWithEncoding(");
        Assert.assertTrue(uploadWithEnc.contains("\"image\",\n        toFormParameterValue(*image),\n        true,\n        \"image/png\""),
                "uploadWithEncoding image must pass 'image/png' as 4th FormParameter arg. "
                + "Method body: " + uploadWithEnc);
        Assert.assertTrue(uploadWithEnc.contains("\"document\",\n        toFormParameterValue(*document),\n        true,\n        \"application/pdf\""),
                "uploadWithEncoding document must pass 'application/pdf' as 4th FormParameter arg. "
                + "Method body: " + uploadWithEnc);
        // description (string, no encoding) — OAS default text/plain.
        Assert.assertTrue(uploadWithEnc.contains("\"description\",\n        toFormParameterValue(*description),\n        false,\n        \"text/plain\""),
                "uploadWithEncoding description (string, no encoding) must pass 'text/plain' as 4th FormParameter arg. "
                + "Method body: " + uploadWithEnc);

        // 2. uploadTextWithEncoding: text/plain as 4th arg on textContent.
        String uploadText = CppBoostBeastTestSupport.extractMethod(apiContent, "uploadTextWithEncoding(");
        Assert.assertTrue(uploadText.contains("\"textContent\",\n        toFormParameterValue(*textContent),\n        false,\n        \"text/plain\""),
                "uploadTextWithEncoding textContent must pass 'text/plain' as 4th FormParameter arg. "
                + "Method body: " + uploadText);
        // notes is binary without encoding — OAS default octet-stream.
        Assert.assertTrue(uploadText.contains("\"notes\",\n        toFormParameterValue(*notes),\n        true,\n        \"application/octet-stream\""),
                "uploadTextWithEncoding notes (binary, no encoding) must pass 'application/octet-stream' as OAS default. "
                + "Method body: " + uploadText);

        // 3. uploadBinaryDefault: rawData (binary, no encoding) gets OAS default.
        String uploadBinary = CppBoostBeastTestSupport.extractMethod(apiContent, "uploadBinaryDefault(");
        Assert.assertTrue(uploadBinary.contains("\"rawData\",\n        toFormParameterValue(*rawData),\n        true,\n        \"application/octet-stream\""),
                "uploadBinaryDefault rawData (binary, no encoding) must pass 'application/octet-stream' as OAS default. "
                + "Method body: " + uploadBinary);

        // 4. uploadMixedEncoding: avatar and report get encoding; signature gets default.
        String uploadMixed = CppBoostBeastTestSupport.extractMethod(apiContent, "uploadMixedEncoding(");
        Assert.assertTrue(uploadMixed.contains("\"avatar\",\n        toFormParameterValue(*avatar),\n        true,\n        \"image/png\""),
                "uploadMixedEncoding avatar must pass 'image/png' as 4th arg. "
                + "Method body: " + uploadMixed);
        Assert.assertTrue(uploadMixed.contains("\"report\",\n        toFormParameterValue(*report),\n        true,\n        \"application/pdf\""),
                "uploadMixedEncoding report must pass 'application/pdf' as 4th arg. "
                + "Method body: " + uploadMixed);
        // signature without encoding — File with OAS default octet-stream.
        Assert.assertTrue(uploadMixed.contains("\"signature\",\n        toFormParameterValue(*signature),\n        true,\n        \"application/octet-stream\""),
                "uploadMixedEncoding signature (binary, no encoding) must pass 'application/octet-stream' as OAS default. "
                + "Method body: " + uploadMixed);

        // 5. uploadJsonObject: payload (object, no encoding) — OAS default application/json.
        String uploadJson = CppBoostBeastTestSupport.extractMethod(apiContent, "uploadJsonObject(");
        Assert.assertTrue(uploadJson.contains("\"payload\",\n        toFormParameterValue(*payload),\n        false,\n        \"application/json\""),
                "uploadJsonObject payload (object, no encoding) must pass 'application/json' as 4th FormParameter arg. "
                + "Method body: " + uploadJson);

        // 6. uploadArrayPart: tags (primitive array, no encoding) — OAS default text/plain.
        String uploadArray = CppBoostBeastTestSupport.extractMethod(apiContent, "uploadArrayPart(");
        Assert.assertTrue(uploadArray.contains("\"tags\",\n        toFormParameterValue(*tags),\n        false,\n        \"text/plain\""),
                "uploadArrayPart tags (primitive array, no encoding) must pass 'text/plain' as 4th FormParameter arg. "
                + "Method body: " + uploadArray);
        // file (binary, no encoding) gets OAS default octet-stream.
        Assert.assertTrue(uploadArray.contains("\"file\",\n        toFormParameterValue(*file),\n        true,\n        \"application/octet-stream\""),
                "uploadArrayPart file (binary, no encoding) must pass 'application/octet-stream' as OAS default. "
                + "Method body: " + uploadArray);
    }

    /**
     * Encoding Object headers are not propagated to multipart parts.
     * The generated code uses only the contentType field from the
     * Encoding Object. A warning is emitted at codegen time when
     * headers are present.
     *
     * <p>See OAS 3.0 §10.4: Encoding Object headers are well-defined
     * but the current implementation does not emit per-part headers.
     * This is acceptable because the Boost.Beast multipart writer
     * constructs parts programmatically and does not expose a
     * per-part header injection API.</p>
     */
    @Test
    public void spaceDelimitedStyleOnFormParamFailsClosed() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();
        CodegenParameter param = new CodegenParameter();
        param.baseName = "tags";
        param.dataType = "std::string";
        param.isFormParam = true;
        param.isSpaceDelimited = true;
        Assert.assertThrows(CppBoostBeastClientCodegen.UnsupportedSchemaAssertionException.class,
                () -> codegen.postProcessParameter(param));
    }

    @Test
    public void pipeDelimitedStyleOnFormParamFailsClosed() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();
        CodegenParameter param = new CodegenParameter();
        param.baseName = "tags";
        param.dataType = "std::string";
        param.isFormParam = true;
        param.isPipeDelimited = true;
        Assert.assertThrows(CppBoostBeastClientCodegen.UnsupportedSchemaAssertionException.class,
                () -> codegen.postProcessParameter(param));
    }

    @Test
    public void deepObjectStyleOnFormParamFailsClosed() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();
        CodegenParameter param = new CodegenParameter();
        param.baseName = "address";
        param.dataType = "std::string";
        param.isFormParam = true;
        param.isDeepObject = true;
        Assert.assertThrows(CppBoostBeastClientCodegen.UnsupportedSchemaAssertionException.class,
                () -> codegen.postProcessParameter(param));
    }

    @Test
    public void normalFormStyleDoesNotFail() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();
        CodegenParameter param = new CodegenParameter();
        param.baseName = "tags";
        param.dataType = "std::string";
        param.isFormParam = true;
        param.isSpaceDelimited = false;
        param.isPipeDelimited = false;
        param.isDeepObject = false;
        // Verifies no exception is thrown for normal form params.
        codegen.postProcessParameter(param);
    }

    @Test
    public void generatesVariantAwareApiIntegration() throws IOException {
        File output = java.nio.file.Files.createTempDirectory("cpp-boost-beast-variant-api").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/composed-schema-lowering.yaml")
                .addAdditionalProperty("sseSchemaMode", "jsonEventData")
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
        String postVariantBodyMethod = CppBoostBeastTestSupport.extractMethod(generatedApiSource, "postVariantBody(");
        Assert.assertTrue(postVariantBodyMethod.contains(
                "serializedRequestBody = boost::json::serialize(toRequestJsonValue(inputParam));"),
                "postVariantBody must serialize using toRequestJsonValue");
        Assert.assertTrue(postVariantBodyMethod.contains(
                "return fromJsonValue_InputParam(responseValue);"),
                "postVariantBody must decode through schema-aware branch selection");

        // Verify postAliasBody method serializes alias body param
        String postAliasBodyMethod = CppBoostBeastTestSupport.extractMethod(generatedApiSource, "postAliasBody(");
        Assert.assertTrue(postAliasBodyMethod.contains(
                "serializedRequestBody = boost::json::serialize(toRequestJsonValue(modelIdsResponses));"),
                "postAliasBody must serialize using toRequestJsonValue");

        // Verify include <optional> and <variant> are present
        Assert.assertTrue(generatedApiSource.contains("#include <optional>"),
                "Generated API source must include <optional>");
        Assert.assertTrue(generatedApiSource.contains("#include <variant>"),
                "Generated API source must include <variant>");

        // Pure SSE uses an incremental typed callback and preserves wire metadata.
        String getStreamEventsMethod = CppBoostBeastTestSupport.extractMethod(
                generatedApiSource, "getStreamEvents(");
        Assert.assertTrue(getStreamEventsMethod.contains("executeStream("));
        Assert.assertTrue(getStreamEventsMethod.contains(
                "[onEvent = std::move(onEvent)](const SseEvent& event) mutable"));
        Assert.assertTrue(getStreamEventsMethod.contains(
                "auto value = fromJsonValue_ResponseStreamEvent(exactEvent.value);"));
        Assert.assertTrue(getStreamEventsMethod.contains("return onEvent(value, event);"));
        Assert.assertTrue(getStreamEventsMethod.contains(
                "HttpResponseData deserializedResponse;"));
        Assert.assertFalse(generatedApiSource.contains("appendParsedEvent"));
        // Verify multipart form-data endpoint generates form parameter handling
        String uploadFileMethod = CppBoostBeastTestSupport.extractMethod(generatedApiSource, "uploadFile(");
        Assert.assertTrue(uploadFileMethod.contains("FormParameter"),
                "uploadFile must generate FormParameter entries");
        Assert.assertTrue(uploadFileMethod.contains("multipart/form-data"),
                "uploadFile must use multipart/form-data serialization");

        // Verify variant form parameter endpoint uses addVariantFormParameter
        String uploadVariantMethod = CppBoostBeastTestSupport.extractMethod(generatedApiSource, "uploadVariantData(");
        Assert.assertTrue(uploadVariantMethod.contains("addVariantFormParameter(formParameters, \"payload\""),
                "uploadVariantData must use addVariantFormParameter for variant form param");
        Assert.assertTrue(uploadVariantMethod.contains("multipart/form-data"),
                "uploadVariantData must use multipart/form-data serialization");

        // Verify VariantPayload model files exist for branch-aware serialization
        Assert.assertTrue(java.nio.file.Files.exists(output.toPath().resolve("model/VariantPayload.h")),
                "VariantPayload model should be generated");
        Assert.assertTrue(java.nio.file.Files.exists(output.toPath().resolve("model/DataObject.h")),
                "DataObject model should be generated");

        // Streaming API header/source signatures both expose response metadata.
        Path apiHeader = output.toPath().resolve("api/ComposedSchemaApi.h");
        String apiHeaderContent = Files.readString(apiHeader);
        Assert.assertTrue(apiHeaderContent.contains("virtual HttpResponseData"));
        Assert.assertTrue(apiHeaderContent.contains(
                "std::function<bool(const ResponseStreamEvent &, const SseEvent &)> onEvent"));
        Assert.assertTrue(apiHeaderContent.contains("getStreamEvents("));

        // Variant headers use toJsonValue_/fromJsonValue_ (not ADL bridge — ADL would conflict)
        String inputParamHeaderContent = Files.readString(
            output.toPath().resolve("model/InputParam.h"));
        Assert.assertFalse(inputParamHeaderContent.contains("to_json("),
                "InputParam header must NOT declare ADL to_json (removed to avoid overload conflict)");
        Assert.assertFalse(inputParamHeaderContent.contains(" from_json("),
                "InputParam header must NOT declare ADL from_json (removed to avoid overload conflict)");

        // ============================================================
        // Strong review: anyOf non-discriminated fixture
        // ============================================================
        // AnyOfStringInteger (anyOf string|integer) → std::variant<std::string, std::int32_t>
        Path anyOfStringIntHeader = output.toPath().resolve("model/AnyOfStringInteger.h");
        TestUtils.assertFileExists(anyOfStringIntHeader);
        String anyOfStringIntContent = java.nio.file.Files.readString(anyOfStringIntHeader);
        Assert.assertTrue(anyOfStringIntContent.contains("using AnyOfStringInteger = std::variant<std::string, std::int32_t>;"),
                "AnyOfStringInteger should be a variant alias to std::variant<std::string, std::int32_t>");

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

        // Dual media types alone are not enough to infer a conditional stream.
        Assert.assertTrue(apiHeaderContent.contains("getDualStream("));
        Assert.assertFalse(apiHeaderContent.contains("getDualStreamStream("),
                "Unconfigured dual-content operations must not gain stream companions");
        Assert.assertFalse(generatedApiSource.contains("getDualStreamStream("));
        // Verify converter name is a valid C++ identifier (no :: or < or shared_ptr)
        Assert.assertFalse(generatedApiSource.contains("fromJsonValue_std::shared_ptr<"),
                "Converter name must not contain std::shared_ptr< (invalid C++ identifier)");
        Assert.assertFalse(generatedApiSource.contains("fromJsonValue_std::"),
                "Converter name must not contain std:: namespace prefix");

        String inlineAnyOfMethod = CppBoostBeastTestSupport.extractMethod(generatedApiSource, "getInlineAnyOfResponse(");
        Assert.assertTrue(inlineAnyOfMethod.contains(
                "fromJsonValue_GetInlineAnyOfResponse_200_response(responseValue)"),
                "Inline anyOf responses must use schema-aware first-match decoding");
        Assert.assertFalse(inlineAnyOfMethod.contains("OneOfResponseBodyDeserializer"),
                "Inline anyOf responses must not use exactly-one decoding");

        String inlineOneOfStreamMethod = CppBoostBeastTestSupport.extractMethod(
                generatedApiSource, "getInlineOneOfEvents(");
        Assert.assertTrue(inlineOneOfStreamMethod.contains(
                "fromJsonValue_GetInlineOneOfEvents_200_response(exactEvent.value)"));
        Assert.assertFalse(generatedApiSource.contains("getDualPrimitiveStreamStream("));

        String noContentMethod = CppBoostBeastTestSupport.extractMethod(generatedApiSource, "deleteWithoutContent(");
        Assert.assertTrue(noContentMethod.contains("status(204)"),
                "No-content operations must handle their successful status");
        Assert.assertTrue(noContentMethod.contains("return;"),
                "Successful no-content operations must return normally");

        String httpClientHeader = Files.readString(output.toPath().resolve("api/HttpClient.h"));
        Assert.assertTrue(httpClientHeader.contains("Streaming is not supported"),
                "Custom HttpClient adapters must inherit a non-pure streaming fallback");
        Assert.assertFalse(httpClientHeader.contains("onEvent) = 0"),
                "executeStream must not remain pure virtual");
        Assert.assertTrue(httpClientHeader.contains("virtual HttpResponseData"),
                "executeStream must return response metadata for error reporting");

        String httpClientSource = Files.readString(
                output.toPath().resolve("api/HttpClientImpl.cpp"));
        Assert.assertTrue(httpClientSource.contains("consumeInitialByteOrderMark"));
        Assert.assertTrue(httpClientSource.contains("http::error::need_buffer"));
        Assert.assertTrue(httpClientSource.contains("appendBoundedBody("),
                "non-SSE and error response bodies must retain the aggregate limit");
        Assert.assertTrue(httpClientSource.contains("isEventStreamContentType"));
        Assert.assertTrue(httpClientSource.contains("streamCancelled"));
        Assert.assertTrue(httpClientSource.contains("tcpStream.expires_never()"));
    }

    @Test
    public void generatesPureSseObjectFixture() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-pure-sse-object").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/pure-sse-object.yaml")
                .addAdditionalProperty("sseSchemaMode", "jsonEventData")
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .forEach(File::deleteOnExit);

        String source = Files.readString(output.toPath().resolve("api/SSEApi.cpp"));
        String header = Files.readString(output.toPath().resolve("api/SSEApi.h"));
        Assert.assertTrue(header.contains("virtual HttpResponseData"));
        Assert.assertTrue(header.contains(
                "std::function<bool(const Evt &, const SseEvent &)> onEvent"));
        Assert.assertTrue(source.contains("fromJsonValue_Evt(exactEvent.value)"));
        Assert.assertTrue(source.contains("return onEvent(value, event);"));
        Assert.assertFalse(source.contains("appendParsedEvent"));
        Assert.assertFalse(source.contains("std::vector<Evt>"));
        Assert.assertTrue(Files.exists(output.toPath().resolve("model/Evt.h")));
    }

    @Test
    public void generatesDualObjectSseFixture() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-dual-object-sse").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/dual-object-sse.yaml")
                .addAdditionalProperty("sseRequestPropertyMappings", "createItem=stream")
                .addAdditionalProperty("sseEventTypeMappings", "createItem=StreamEvent")
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .forEach(File::deleteOnExit);

        String source = Files.readString(output.toPath().resolve("api/DualApi.cpp"));
        String header = Files.readString(output.toPath().resolve("api/DualApi.h"));
        Assert.assertTrue(header.contains("createItemStream("));
        Assert.assertTrue(header.contains(
                "std::function<bool(const StreamEvent &, const SseEvent &)> onEvent"));
        Assert.assertTrue(source.contains("conditionalSseRequestBody->setStream(false);"));
        Assert.assertTrue(source.contains("conditionalSseRequestBody->setStream(true);"));
        Assert.assertTrue(source.contains(
                "toRequestJsonValue(conditionalSseRequestBody)"));
        Assert.assertTrue(source.contains(
                "ResponseJsonValueConverter<StreamEvent>::convert(exactEvent.value)"));
        Assert.assertTrue(source.contains("headers[\"Accept\"] = \"text/event-stream\";"));
        Assert.assertTrue(source.contains("streamOptions);"));
    }

    @Test
    public void streamsPureSseWithoutResponseSchema() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-pure-sse-no-schema").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/pure-sse-no-schema.yaml")
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .forEach(File::deleteOnExit);

        String header = Files.readString(output.toPath().resolve("api/SSEApi.h"));
        String source = Files.readString(output.toPath().resolve("api/SSEApi.cpp"));
        Assert.assertTrue(header.contains("virtual HttpResponseData"));
        Assert.assertTrue(header.contains("SseEventCallback onEvent"));
        Assert.assertFalse(header.contains("std::vector<void>"));
        Assert.assertTrue(source.contains("m_client->executeStream("));
    }

    @Test
    public void pureSseObjectInRepresentationModeUsesStructuredCallback() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-sse-repr").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/pure-sse-object.yaml")
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .forEach(File::deleteOnExit);

        String source = Files.readString(output.toPath().resolve("api/SSEApi.cpp"));
        String header = Files.readString(output.toPath().resolve("api/SSEApi.h"));
        Assert.assertTrue(header.contains("SseEventCallback onEvent"));
        Assert.assertTrue(source.contains("std::move(onEvent),"));
        Assert.assertTrue(source.contains("m_client->executeStream("));
        Assert.assertFalse(source.contains("parseExactJson(event.data)"));
        Assert.assertFalse(source.contains("std::vector<std::string> deserializedResponse"));
    }

    @Test
    public void pureSseObjectInJsonEventDataModeUsesTypedCallback() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-sse-typed").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/pure-sse-object.yaml")
                .addAdditionalProperty("sseSchemaMode", "jsonEventData")
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .forEach(File::deleteOnExit);

        String source = Files.readString(output.toPath().resolve("api/SSEApi.cpp"));
        String header = Files.readString(output.toPath().resolve("api/SSEApi.h"));
        Assert.assertTrue(header.contains(
                "std::function<bool(const Evt &, const SseEvent &)> onEvent"));
        Assert.assertTrue(source.contains("parseExactJson(event.data)"));
        Assert.assertTrue(source.contains("fromJsonValue_Evt(exactEvent.value)"));
        Assert.assertTrue(source.contains("if (event.data == \"[DONE]\") return false;"));
        Assert.assertFalse(source.contains("appendParsedEvent"));
    }

    @Test
    public void dualContentObjectInRepresentationModeUsesStructuredCallback()
            throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-dual-repr").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/dual-object-sse.yaml")
                .addAdditionalProperty("sseRequestPropertyMappings", "createItem=stream")
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .forEach(File::deleteOnExit);

        String source = Files.readString(output.toPath().resolve("api/DualApi.cpp"));
        String header = Files.readString(output.toPath().resolve("api/DualApi.h"));
        Assert.assertTrue(header.contains("createItemStream("));
        Assert.assertTrue(header.contains("SseEventCallback onEvent"));
        Assert.assertTrue(source.contains("conditionalSseRequestBody->setStream(true);"));
        Assert.assertTrue(source.contains("std::move(onEvent),"));
        Assert.assertFalse(source.contains("parseExactJson(event.data)"));
    }

    @Test
    public void explicitEventTypeMappingEnablesTypedPathUnderDefaultMode()
            throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-sse-event-map").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/pure-sse-object.yaml")
                .addAdditionalProperty("sseEventTypeMappings", "getEvents=Evt")
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .forEach(File::deleteOnExit);

        String source = Files.readString(output.toPath().resolve("api/SSEApi.cpp"));
        String header = Files.readString(output.toPath().resolve("api/SSEApi.h"));
        Assert.assertTrue(header.contains(
                "std::function<bool(const Evt &, const SseEvent &)> onEvent"));
        Assert.assertTrue(source.contains("fromJsonValue_Evt(exactEvent.value)"));
    }

    @Test
    public void perOperationEventDataExtensionUsesTypedCallback() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-sse-perop").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/pure-sse-with-event-data-schema.yaml")
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .forEach(File::deleteOnExit);

        String source = Files.readString(output.toPath().resolve("api/SSEApi.cpp"));
        String header = Files.readString(output.toPath().resolve("api/SSEApi.h"));
        Assert.assertTrue(header.contains(
                "std::function<bool(const TypedEvent &, const SseEvent &)> onEvent"));
        Assert.assertTrue(source.contains("fromJsonValue_TypedEvent(exactEvent.value)"));
        Assert.assertTrue(Files.exists(output.toPath().resolve("model/TypedEvent.h")));
    }

    @Test
    public void infersUniqueSseRequestPropertyAndEventModel() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-sse-inferred").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/dual-object-sse.yaml")
                .addAdditionalProperty("sseSchemaMode", "jsonEventData")
                .setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate()
                .forEach(File::deleteOnExit);

        String source = Files.readString(output.toPath().resolve("api/DualApi.cpp"));
        String header = Files.readString(output.toPath().resolve("api/DualApi.h"));
        Assert.assertTrue(header.contains("createItemStream("));
        Assert.assertTrue(header.contains(
                "std::function<bool(const StreamEvent &, const SseEvent &)> onEvent"));
        Assert.assertTrue(source.contains("conditionalSseRequestBody->setStream(true);"));
        Assert.assertTrue(source.contains("if (event.data == \"[DONE]\") return false;"));
        Assert.assertFalse(header.contains("createAmbiguousStream("));
    }

    @Test
    public void rejectsMalformedSseMetadataMappings() {
        for (String option : List.of(
                "sseRequestPropertyMappings", "sseEventTypeMappings")) {
            CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
            codegen.additionalProperties().put(option, "getEvents");

            IllegalArgumentException exception = Assert.expectThrows(
                    IllegalArgumentException.class, codegen::processOpts);
            Assert.assertTrue(exception.getMessage().contains(option));
            Assert.assertTrue(exception.getMessage().contains("operationId=value"));
        }
    }

    @Test
    public void rejectsInvalidSseOptionValues() {
        CppBoostBeastClientCodegen invalidMode = new CppBoostBeastClientCodegen();
        invalidMode.additionalProperties().put("sseSchemaMode", "events");
        IllegalArgumentException modeException = Assert.expectThrows(
                IllegalArgumentException.class, invalidMode::processOpts);
        Assert.assertTrue(modeException.getMessage().contains("sseSchemaMode"));

        CppBoostBeastClientCodegen invalidInference = new CppBoostBeastClientCodegen();
        invalidInference.additionalProperties().put(
                "inferConditionalSseOperations", "sometimes");
        IllegalArgumentException inferenceException = Assert.expectThrows(
                IllegalArgumentException.class, invalidInference::processOpts);
        Assert.assertTrue(inferenceException.getMessage().contains(
                "inferConditionalSseOperations"));
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
            CppBoostBeastTestSupport.assertBalancedPreprocessorGuards(header);
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
        Assert.assertTrue(treeContent.contains("std::vector<std::vector<std::shared_ptr<TreeNode>>>"),
                "Nested self-ref TreeNode items should retain shared_ptr");

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

    @Test
    public void formatAssertionPolicyDefaultsToAnnotation() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();
        Assert.assertEquals(codegen.additionalProperties().get("formatAssertionPolicy"),
                "annotation",
                "formatAssertionPolicy must default to annotation");
    }

    @Test
    public void formatAssertionPolicyRejectsUnimplementedStrictMode() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.additionalProperties().put("formatAssertionPolicy", "strict");
        Assert.assertThrows(IllegalArgumentException.class, codegen::processOpts);
    }

    @Test
    public void tolerateNonNullableNullsDefaultsOnAndCanBeDisabled() throws IOException {
        CppBoostBeastClientCodegen defaults = new CppBoostBeastClientCodegen();
        defaults.processOpts();
        Assert.assertEquals(defaults.additionalProperties().get("tolerateNonNullableNulls"),
                true,
                "Generated clients must tolerate non-conforming null responses by default");

        Path testOutputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(testOutputRoot, "cpp-boost-beast-null-tolerance-");
        output.toFile().deleteOnExit();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/model-generation-regression.yaml")
                .setOutputDir(output.toString());
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Assert.assertFalse(files.isEmpty(), "generation must produce files");

        String source = Files.readString(output.resolve("model/ContainerModel.cpp"));
        Assert.assertTrue(source.contains("if (OptionalScalarIt != object.end())"));
        Assert.assertTrue(source.contains("if (!OptionalScalarIt->value().is_null())"),
                "Default decoding must treat an optional non-nullable null as absent");
        Assert.assertTrue(source.contains("if (RequiredValueIt != object.end())"));
        Assert.assertTrue(source.contains("if (!RequiredValueIt->value().is_null())"),
                "A present required key with null must be tolerated by default");
        Assert.assertTrue(source.contains(
                "Required field 'requiredValue' not found in ContainerModel"),
                "Compatibility mode must still reject a missing required key");

        Path strictOutput = Files.createTempDirectory(
                testOutputRoot, "cpp-boost-beast-strict-null-");
        strictOutput.toFile().deleteOnExit();
        CodegenConfigurator strictConfigurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/oas31-runtime-regression.yaml")
                .addAdditionalProperty("tolerateNonNullableNulls", "false")
                .setOutputDir(strictOutput.toString());
        List<File> strictFiles = new DefaultGenerator()
                .opts(strictConfigurator.toClientOptInput()).generate();
        Assert.assertFalse(strictFiles.isEmpty(), "strict generation must produce files");
        String strictResponseSource = Files.readString(
                strictOutput.resolve("model/NullDriftResponse.cpp"));
        Assert.assertFalse(strictResponseSource.contains("if (!UserIt->value().is_null())"),
                "Explicit strict decoding must reject a non-nullable null");
        String strictEventSource = Files.readString(
                strictOutput.resolve("model/NullDriftEvent.cpp"));
        Assert.assertFalse(strictEventSource.contains(
                        "tolerateNonNullablePropertyNulls = true"),
                "Explicit strict decoding must not relax composition validation");
    }
    @Test
    public void preserveAdditionalPropertiesDefaultsOffAndEmitsExtrasStorage() throws IOException {
        CppBoostBeastClientCodegen defaults = new CppBoostBeastClientCodegen();
        defaults.processOpts();
        Assert.assertEquals(defaults.additionalProperties().get("preserveAdditionalProperties"),
                false,
                "Generated models must discard undeclared fields unless preservation is enabled");

        CppBoostBeastClientCodegen invalid = new CppBoostBeastClientCodegen();
        invalid.additionalProperties().put("preserveAdditionalProperties", "sometimes");
        Assert.assertThrows(IllegalArgumentException.class, invalid::processOpts);

        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(
                outputRoot, "cpp-boost-beast-preserve-additional-properties-");
        output.toFile().deleteOnExit();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/"
                        + "preserve-additional-properties.yaml")
                .setOutputDir(output.toString())
                .addAdditionalProperty("preserveAdditionalProperties", true);
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Assert.assertFalse(files.isEmpty(), "generation must produce files");

        String header = Files.readString(output.resolve("model/ExtraFields.h"));
        String source = Files.readString(output.resolve("model/ExtraFields.cpp"));
        Assert.assertTrue(header.contains(
                        "using ExtraJsonProperties = std::map<std::string, boost::json::value>;"),
                "Enabled preservation must expose the extra JSON property storage type");
        Assert.assertTrue(header.contains("getExtraJsonProperties2() const noexcept")
                        && header.contains("setExtraJsonProperties2(ExtraJsonProperties extraJsonProperties)"),
                "Preservation map accessors must avoid generated property accessors");
        Assert.assertTrue(header.contains("std::string getExtraJsonProperties() const;"),
                "A declared property must retain its original accessor name");
        Assert.assertTrue(source.contains("m_extraJsonProperties2.clear()")
                        && source.contains("isKnownJsonProperty(member.key())")
                        && source.contains("object[name] = extraJsonProperty.second"),
                "Enabled preservation must capture only unknown fields and re-emit them");

        String variantSource = Files.readString(output.resolve("model/ExtraFieldsVariant.cpp"));
        Assert.assertTrue(variantSource.contains("tolerateAdditionalProperties = true"),
                "Enabled preservation must relax composition validation before model decoding");

        Path strictOutput = Files.createTempDirectory(
                outputRoot, "cpp-boost-beast-discard-additional-properties-");
        strictOutput.toFile().deleteOnExit();
        CodegenConfigurator strictConfigurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/"
                        + "preserve-additional-properties.yaml")
                .setOutputDir(strictOutput.toString());
        new DefaultGenerator().opts(strictConfigurator.toClientOptInput()).generate();
        String strictHeader = Files.readString(strictOutput.resolve("model/ExtraFields.h"));
        Assert.assertFalse(strictHeader.contains(
                        "using ExtraJsonProperties = std::map<std::string, boost::json::value>;"),
                "Default generation must not add extra JSON property storage");
    }

    @Test
    public void emitsOptInSseEofCompatibilityPolicy() throws IOException {
        Path outputRoot = Files.createDirectories(Path.of("target"));
        Path output = Files.createTempDirectory(outputRoot, "cpp-boost-beast-sse-eof-");
        output.toFile().deleteOnExit();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/"
                        + "preserve-additional-properties.yaml")
                .setOutputDir(output.toString());
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        String header = Files.readString(output.resolve("api/HttpClient.h"));
        String source = Files.readString(output.resolve("api/HttpClientImpl.cpp"));
        Assert.assertTrue(header.contains("bool dispatchUnterminatedEventAtEof = false;"),
                "SSE EOF compatibility must default to strict WHATWG framing");
        Assert.assertTrue(source.contains(
                        "if (m_options.dispatchUnterminatedEventAtEof && !m_pending.empty())")
                        && source.contains(
                        "processLine(std::string_view(m_pending.data(), m_pending.size()))")
                        && source.contains(
                        "if (m_options.dispatchUnterminatedEventAtEof) dispatchEvent();"),
                "Enabled SSE EOF compatibility must parse and dispatch the final partial event");
    }

    @Test
    public void compileWithValidationDefaultsToTrue() throws IOException {
        // Default: kValidateOnDecode = true in generated ValidationTypes.h.
        File output = Files.createTempDirectory("cpp-boost-beast-knob-default").toFile();
        output.deleteOnExit();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/composed-schema-lowering.yaml")
                .setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Assert.assertFalse(files.isEmpty(), "generation must produce files");
        Path validationTypes = output.toPath().resolve("model/ValidationTypes.h");
        Assert.assertTrue(Files.exists(validationTypes),
                "model/ValidationTypes.h must be emitted");
        String content = new String(Files.readAllBytes(validationTypes), java.nio.charset.StandardCharsets.UTF_8);
        Assert.assertTrue(content.contains("constexpr bool kValidateOnDecode = true;"),
                "compileWithValidation default must emit kValidateOnDecode = true");
    }

    @Test
    public void compileWithValidationFalseEmitsKnobOff() throws IOException {
        File output = Files.createTempDirectory("cpp-boost-beast-knob-off").toFile();
        output.deleteOnExit();
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/composed-schema-lowering.yaml")
                .addAdditionalProperty("compileWithValidation", "false")
                .setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Assert.assertFalse(files.isEmpty(), "generation must produce files");
        Path validationTypes = output.toPath().resolve("model/ValidationTypes.h");
        Assert.assertTrue(Files.exists(validationTypes),
                "model/ValidationTypes.h must be emitted");
        String content = new String(Files.readAllBytes(validationTypes), java.nio.charset.StandardCharsets.UTF_8);
        Assert.assertTrue(content.contains("constexpr bool kValidateOnDecode = false;"),
                "compileWithValidation=false must emit kValidateOnDecode = false");

        Path modelDirectory = output.toPath().resolve("model");
        for (String omittedFile : List.of(
                "Oas31SchemaRegistry.h",
                "schema_ir.generated.cpp")) {
            Assert.assertFalse(Files.exists(modelDirectory.resolve(omittedFile)),
                    omittedFile + " must not be emitted when validation is disabled");
        }
        try (java.util.stream.Stream<Path> modelFiles = Files.list(modelDirectory)) {
            Assert.assertFalse(modelFiles.anyMatch(path -> path.getFileName().toString()
                            .startsWith("schema_ir.generated.chunk")),
                    "Schema IR chunk translation units must not be emitted when validation is disabled");
        }

        String compositionSource = Files.readString(modelDirectory.resolve("AnyOfOverlapping.cpp"));
        Assert.assertFalse(compositionSource.contains("bool validate_"),
                "Validation-disabled model sources must not define per-branch validators");
        Assert.assertFalse(compositionSource.contains("#include \"Oas31SchemaRegistry.h\""),
                "Validation-disabled model sources must not include the stripped registry");
        Assert.assertFalse(compositionSource.contains("#include \"Oas31Validator.h\""),
                "Validation-disabled model sources must not include the schema evaluator");

        String cmakeLists = Files.readString(output.toPath().resolve("CMakeLists.txt"));
        Assert.assertFalse(cmakeLists.contains("schema_ir.generated"),
                "Generated CMake must not reference stripped schema IR sources");
        Assert.assertFalse(cmakeLists.contains("Oas31SchemaRegistry.h"),
                "Generated CMake must not reference the stripped schema registry");

        for (String retainedHeader : List.of(
                "Oas31ExactNumber.h",
                "Oas31SchemaIr.h",
                "Oas31Validator.h")) {
            Assert.assertTrue(Files.exists(modelDirectory.resolve(retainedHeader)),
                    retainedHeader + " must remain available as a header-only utility");
        }
    }

    @Test
    public void featureSetReflectsWave5Deliverables() {
        // Parameter styling, multi-server, cookie params, and `not` schema
        // validation are delivered and assertable; callbacks/link objects are
        // preserved as metadata; XML structure definitions stay excluded.
        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        openAPI.setServers(new java.util.ArrayList<>());
        openAPI.setComponents(new io.swagger.v3.oas.models.Components());
        openAPI.setPaths(new io.swagger.v3.oas.models.Paths());
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        FeatureSet features = codegen.getFeatureSet();
        java.util.Set<GlobalFeature> globals = features.getGlobalFeatures();
        Assert.assertTrue(globals.contains(GlobalFeature.ParameterStyling),
                "ParameterStyling must be delivered");
        Assert.assertTrue(globals.contains(GlobalFeature.MultiServer),
                "MultiServer must be delivered");
        Assert.assertTrue(globals.contains(GlobalFeature.Callbacks),
                "Callbacks must be preserved as metadata");
        Assert.assertTrue(globals.contains(GlobalFeature.LinkObjects),
                "LinkObjects must be preserved as metadata");
        Assert.assertTrue(features.getParameterFeatures()
                .contains(ParameterFeature.Cookie),
                "Cookie params must be delivered");
        Assert.assertTrue(features.getSchemaSupportFeatures()
                .contains(SchemaSupportFeature.not),
                "not schema validation must be advertised");
        Assert.assertFalse(globals.contains(GlobalFeature.XMLStructureDefinitions),
                "XMLStructureDefinitions stays excluded");
    }

    @Test
    public void callbacksAndResponseLinksArePreservedFromParsedModel() throws IOException {
        Path workspace = Files.createTempDirectory(
                Files.createDirectories(Path.of("target")),
                "cpp-boost-beast-operation-metadata");
        Path spec = workspace.resolve("input.yaml");
        Files.writeString(spec,
                "openapi: 3.1.0\n"
              + "info: {title: metadata, version: 1.0.0}\n"
              + "paths:\n"
              + "  /jobs:\n"
              + "    post:\n"
              + "      operationId: startJob\n"
              + "      callbacks:\n"
              + "        onResult:\n"
              + "          '{$request.body#/callbackUrl}':\n"
              + "            post:\n"
              + "              responses:\n"
              + "                '204': {description: accepted}\n"
              + "      responses:\n"
              + "        '202':\n"
              + "          description: accepted\n"
              + "          links:\n"
              + "            pollJob:\n"
              + "              operationId: getJob\n"
              + "  /jobs/{id}:\n"
              + "    get:\n"
              + "      operationId: getJob\n"
              + "      parameters:\n"
              + "        - {name: id, in: path, required: true, schema: {type: string}}\n"
              + "      responses:\n"
              + "        '200': {description: ok}\n");
        File output = workspace.resolve("generated").toFile();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec(spec.toString())
                .setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator()
                .opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path source = output.toPath().resolve("api/DefaultApi.cpp");
        TestUtils.assertFileContains(source,
                "Callback metadata preserved; no inbound listener is generated: onResult",
                "Link metadata preserved; no automatic traversal is generated: pollJob");
    }

    @Test
    public void webhooksArePreservedAndDoNotSuppressPathOperations() {
        // Webhooks are inbound-only metadata for a client
        // generator. Upstream folds them into the api map under the same
        // fallback classname, silently REPLACING the paths api; the codegen
        // preserves the metadata and strips them so the paths generate.
        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        openAPI.setServers(new java.util.ArrayList<>());
        io.swagger.v3.oas.models.Components components =
                new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new java.util.LinkedHashMap<>();
        schemas.put("Ping", new Schema().type("object"));
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        io.swagger.v3.oas.models.Paths paths = new io.swagger.v3.oas.models.Paths();
        io.swagger.v3.oas.models.Operation getOp = new io.swagger.v3.oas.models.Operation();
        getOp.setOperationId("getPing");
        getOp.responses(new io.swagger.v3.oas.models.responses.ApiResponses()
                .addApiResponse("200", new io.swagger.v3.oas.models.responses.ApiResponse()
                        .description("ok").content(new io.swagger.v3.oas.models.media.Content()
                                .addMediaType("application/json",
                                        new io.swagger.v3.oas.models.media.MediaType()
                                                .schema(new io.swagger.v3.oas.models.media.ObjectSchema())))));
        paths.addPathItem("/ping",
                new io.swagger.v3.oas.models.PathItem().get(getOp));
        openAPI.setPaths(paths);
        io.swagger.v3.oas.models.Operation hookOp = new io.swagger.v3.oas.models.Operation();
        hookOp.setOperationId("newEventPost");
        hookOp.responses(new io.swagger.v3.oas.models.responses.ApiResponses()
                .addApiResponse("200", new io.swagger.v3.oas.models.responses.ApiResponse()
                        .description("ok")));
        java.util.Map<String, io.swagger.v3.oas.models.PathItem> webhooks =
                new java.util.LinkedHashMap<>();
        webhooks.put("newEvent",
                new io.swagger.v3.oas.models.PathItem().post(hookOp));
        openAPI.setWebhooks(webhooks);

        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.getWebhookPreservation().size(), 1,
                "the webhook metadata must be preserved");
        Assert.assertTrue(
                codegen.getWebhookPreservation().get(0).contains("newEvent[POST newEventPost]"),
                "preserved metadata must name the webhook + method + operationId: "
                        + codegen.getWebhookPreservation());
        Assert.assertNull(openAPI.getWebhooks(),
                "webhooks must be stripped from generation (paths api must survive)");
    }

    @Test
    public void generatedOutputContainsNoInternalVendorMarkers() throws IOException {
        // Internal engine channels (x-oas31-* recovery markers, x-cpp-*
        // template plumbing, x-codegen-* bookkeeping) must never leak into
        // the generated client. The composition fixture exercises the marker
        // machinery on oneOf/anyOf/discriminator surfaces.
        File output = java.nio.file.Files.createTempDirectory(
                "cpp-boost-beast-no-leak").toFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/fixtures/composition-fixtures.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("packageName", "CppBoostBeastNoLeakTest");

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        java.util.List<String> offenders = new java.util.ArrayList<>();
        try (java.util.stream.Stream<Path> paths = Files.walk(output.toPath())) {
            java.util.Iterator<Path> it = paths.iterator();
            while (it.hasNext()) {
                Path p = it.next();
                if (!Files.isRegularFile(p)) continue;
                String content = Files.readString(p);
                for (String marker : new String[] {
                        "x-oas31-", "x-cpp-", "x-codegen-"}) {
                    if (content.contains(marker)) {
                        offenders.add(p.getFileName() + " contains " + marker);
                    }
                }
            }
        }
        Assert.assertTrue(offenders.isEmpty(),
                "generated output must not carry internal vendor markers: "
                        + String.join("; ", offenders));
    }
}