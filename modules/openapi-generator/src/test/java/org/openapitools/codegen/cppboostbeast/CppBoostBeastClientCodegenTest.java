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

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
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
                "boost::asio::ssl::host_name_verification(m_host)",
                "request.method_string(verb)",
                "target must use HTTP origin-form",
                "header name is reserved by the transport");
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
                "errorMessage << \"Value \" << value << \" not allowed\";",
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

}
