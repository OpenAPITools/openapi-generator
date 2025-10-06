/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

package org.openapitools.codegen.python;

import com.google.common.collect.Sets;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.PythonClientCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileExists;

public class PythonClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test(description = "test enum null/nullable patterns")
    public void testEnumNull() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_1997.yaml");

        StringSchema prop = (StringSchema) openAPI.getComponents().getSchemas().get("Type").getProperties().get("prop");
        ArrayList<Object> expected = new ArrayList<>(Arrays.asList("A", "B", "C"));
        assert prop.getNullable();
        assert prop.getEnum().equals(expected);
    }

    @Test(description = "test regex patterns")
    public void testRegularExpressionOpenAPISchemaVersion3() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_1517.yaml");
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/ping";
        final Operation p = openAPI.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, null);
        // pattern_no_forward_slashes '^pattern$'
        Assert.assertEquals(op.allParams.get(0).pattern, "/^pattern$/");
        // pattern_two_slashes '/^pattern$/'
        Assert.assertEquals(op.allParams.get(1).pattern, "/^pattern$/");
        // pattern_dont_escape_backslash '/^pattern\d{3}$/'
        Assert.assertEquals(op.allParams.get(2).pattern, "/^pattern\\d{3}$/");
        // pattern_dont_escape_escaped_forward_slash '/^pattern\/\d{3}$/'
        Assert.assertEquals(op.allParams.get(3).pattern, "/^pattern\\/\\d{3}$/");
        // pattern_escape_unescaped_forward_slash '^pattern/\d{3}$'
        Assert.assertEquals(op.allParams.get(4).pattern, "/^pattern\\/\\d{3}$/");
        // pattern_with_modifiers '/^pattern\d{3}$/i
        Assert.assertEquals(op.allParams.get(5).pattern, "/^pattern\\d{3}$/i");
        // pattern_with_backslash_after_bracket '/^[\pattern\d{3}$/i'
        // added to test fix for issue #6675
        // removed because "/^[\\pattern\\d{3}$/i" is invalid regex because [ is not escaped and there is no closing ]
        // Assert.assertEquals(op.allParams.get(6).pattern, "/^[\\pattern\\d{3}$/i");

    }


    @Test(description = "test generated example values for string properties")
    public void testGeneratedExampleValues() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/examples.yaml");
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);
        final Schema dummyUserSchema = openAPI.getComponents().getSchemas().get("DummyUser");
        final Schema nameSchema = (Schema) dummyUserSchema.getProperties().get("name");
        final Schema numberSchema = (Schema) dummyUserSchema.getProperties().get("number");
        final Schema addressSchema = (Schema) dummyUserSchema.getProperties().get("address");
        final String namePattern = codegen.patternCorrection(nameSchema.getPattern());
        final String numberPattern = codegen.patternCorrection(numberSchema.getPattern());
        final String addressPattern = codegen.patternCorrection(addressSchema.getPattern());
        Assert.assertTrue(codegen.escapeQuotationMark(codegen.toExampleValue(nameSchema)).matches(namePattern));
        Assert.assertTrue(codegen.escapeQuotationMark(codegen.toExampleValue(numberSchema)).matches(numberPattern));
        Assert.assertTrue(codegen.escapeQuotationMark(codegen.toExampleValue(addressSchema)).matches(addressPattern));
    }

    @Test(description = "test single quotes escape")
    public void testSingleQuotes() {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        StringSchema schema = new StringSchema();
        schema.setDefault("Text containing 'single' quote");
        String defaultValue = codegen.toDefaultValue(schema);
        Assert.assertEquals("'Text containing \\'single\\' quote'", defaultValue);
    }

    @Test(description = "test backslash default")
    public void testBackslashDefault() {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        StringSchema schema = new StringSchema();
        schema.setDefault("\\");
        String defaultValue = codegen.toDefaultValue(schema);
        Assert.assertEquals("'\\\\'", defaultValue);
    }

    @Test(description = "convert a python model with dots")
    public void modelTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/v1beta3.yaml");
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);

        codegen.setOpenAPI(openAPI);
        final CodegenModel simpleName = codegen.fromModel("v1beta3.Binding", openAPI.getComponents().getSchemas().get("v1beta3.Binding"));
        Assert.assertEquals(simpleName.name, "v1beta3.Binding");
        Assert.assertEquals(simpleName.classname, "V1beta3Binding");
        Assert.assertEquals(simpleName.classVarName, "v1beta3_binding");

        codegen.setOpenAPI(openAPI);
        final CodegenModel compoundName = codegen.fromModel("v1beta3.ComponentStatus", openAPI.getComponents().getSchemas().get("v1beta3.ComponentStatus"));
        Assert.assertEquals(compoundName.name, "v1beta3.ComponentStatus");
        Assert.assertEquals(compoundName.classname, "V1beta3ComponentStatus");
        Assert.assertEquals(compoundName.classVarName, "v1beta3_component_status");

        final String path = "/api/v1beta3/namespaces/{namespaces}/bindings";
        final Operation operation = openAPI.getPaths().get(path).getPost();
        final CodegenOperation codegenOperation = codegen.fromOperation(path, "get", operation, null);
        Assert.assertEquals(codegenOperation.returnType, "V1beta3Binding");
        Assert.assertEquals(codegenOperation.returnBaseType, "V1beta3Binding");
    }

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "str");
        Assert.assertEquals(property2.name, "name");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "str");
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "datetime");
        Assert.assertEquals(property3.name, "created_at");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "datetime");
        Assert.assertFalse(property3.required);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.dataType, "List[str]");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "List");
        Assert.assertEquals(property2.containerType, "array");
        Assert.assertFalse(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertTrue(property2.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "translations");
        Assert.assertEquals(property1.dataType, "Dict[str, str]");
        Assert.assertEquals(property1.name, "translations");
        Assert.assertEquals(property1.baseType, "Dict");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
        Assert.assertTrue(property1.isPrimitiveType);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.dataType, "Children");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "Children");
        Assert.assertFalse(property1.required);
        Assert.assertFalse(property1.isContainer);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.dataType, "List[Children]");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "List");
        Assert.assertEquals(property1.containerType, "array");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.dataType, "Dict[str, Children]");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "Dict");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
    }


    // should not start with 'null'. need help from the community to investigate further
    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema model = new ArraySchema()
                //.description()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, "null<Children>");
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    // should not start with 'null'. need help from the community to investigate further
    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema model = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, null);
        Assert.assertEquals(cm.imports.size(), 0);
    }

    @Test(description = "check API example has input param(configuration) when it creates api_client")
    public void apiExampleDocTest() throws Exception {
        final DefaultCodegen codegen = new PythonClientCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/generic.yaml");
        final Path p = Paths.get(outputPath + "docs/DefaultApi.md");

        assertFileExists(p);
        assertFileContains(p, "openapi_client.ApiClient(configuration) as api_client");
    }

    // Helper function, intended to reduce boilerplate
    static private String generateFiles(DefaultCodegen codegen, String filePath) throws IOException {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final String outputPath = output.getAbsolutePath().replace('\\', '/');

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        final ClientOptInput input = new ClientOptInput();
        final OpenAPI openAPI = new OpenAPIParser().readLocation(filePath, null, new ParseOptions()).getOpenAPI();
        input.openAPI(openAPI);
        input.config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        Assert.assertTrue(files.size() > 0);
        return outputPath + "/";
    }

    @Test(description = "test containerType in parameters")
    public void testContainerType() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);
        // path parameter
        String path = "/store/order/{orderId}";
        Operation p = openAPI.getPaths().get(path).getGet();
        CodegenOperation op = codegen.fromOperation(path, "get", p, null);
        Assert.assertEquals(op.allParams.get(0).containerType, null);
        Assert.assertEquals(op.allParams.get(0).baseName, "orderId");

        // query parameter
        path = "/user/login";
        p = openAPI.getPaths().get(path).getGet();
        op = codegen.fromOperation(path, "get", p, null);
        Assert.assertEquals(op.allParams.get(0).containerType, null);
        Assert.assertEquals(op.allParams.get(0).baseName, "username");
        Assert.assertEquals(op.allParams.get(1).containerType, null);
        Assert.assertEquals(op.allParams.get(1).baseName, "password");

        // body parameter
        path = "/user/createWithList";
        p = openAPI.getPaths().get(path).getPost();
        op = codegen.fromOperation(path, "post", p, null);
        Assert.assertEquals(op.allParams.get(0).baseName, "User");
        Assert.assertEquals(op.allParams.get(0).containerType, "array");
        Assert.assertEquals(op.allParams.get(0).containerTypeMapped, "List");

        path = "/pet";
        p = openAPI.getPaths().get(path).getPost();
        op = codegen.fromOperation(path, "post", p, null);
        Assert.assertEquals(op.allParams.get(0).baseName, "Pet");
        Assert.assertEquals(op.allParams.get(0).containerType, null);
        Assert.assertEquals(op.allParams.get(0).containerTypeMapped, null);

    }

    @Test(description = "test containerType (dict) in parameters")
    public void testContainerTypeForDict() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/dict_query_parameter.yaml");
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);
        // query parameter
        String path = "/query_parameter_dict";
        Operation p = openAPI.getPaths().get(path).getGet();
        CodegenOperation op = codegen.fromOperation(path, "get", p, null);
        Assert.assertEquals(op.allParams.get(0).containerType, "map");
        Assert.assertEquals(op.allParams.get(0).containerTypeMapped, "Dict");
        Assert.assertEquals(op.allParams.get(0).baseName, "dict_string_integer");
    }

    @Test(description = "convert a model with dollar signs")
    public void modelTestDollarSign() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/dollar-in-names-pull14359.yaml");
        final DefaultCodegen codegen = new PythonClientCodegen();

        codegen.setOpenAPI(openAPI);
        final CodegenModel simpleName = codegen.fromModel("$DollarModel$", openAPI.getComponents().getSchemas().get("$DollarModel$"));
        Assert.assertEquals(simpleName.name, "$DollarModel$");
        Assert.assertEquals(simpleName.classname, "DollarModel");
        Assert.assertEquals(simpleName.classVarName, "dollar_model");

        List<CodegenProperty> vars = simpleName.getVars();
        Assert.assertEquals(vars.size(), 1);
        CodegenProperty property = vars.get(0);
        Assert.assertEquals(property.name, "dollar_value");
    }

    @Test
    public void testHandleConstantParams() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/java/autoset_constant.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        PythonClientCodegen pythonClientCodegen = new PythonClientCodegen();
        pythonClientCodegen.setOutputDir(output.getAbsolutePath());
        pythonClientCodegen.additionalProperties().put(CodegenConstants.AUTOSET_CONSTANTS, "true");
        pythonClientCodegen.setAutosetConstants(true);
        clientOptInput.config(pythonClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        File apiFile = files
                .get(Paths.get(output.getAbsolutePath(), "openapi_client", "api", "hello_example_api.py").toString());
        assertNotNull(apiFile);
        assertFileContains(apiFile.toPath(), "_header_params['X-CUSTOM_CONSTANT_HEADER'] = 'CONSTANT_VALUE'");
        assertFileContains(apiFile.toPath(), "_query_params.append(('CONSTANT_QUERY_STRING_KEY', 'CONSTANT_QUERY_STRING_VALUE'))");
    }

    @Test(description = "Enum value with quotes (#17582)")
    public void testEnumPropertyWithQuotes() {
        final PythonClientCodegen codegen = new PythonClientCodegen();

        Assert.assertEquals(codegen.toEnumValue("enum-value", "string"), "'enum-value'");
        Assert.assertEquals(codegen.toEnumValue("won't fix", "string"), "'won\\'t fix'");
        Assert.assertEquals(codegen.toEnumValue("\"", "string"), "'\\\"'");
        Assert.assertEquals(codegen.toEnumValue("1.0", "float"), "1.0");
        Assert.assertEquals(codegen.toEnumValue("1", "int"), "1");
    }

    @Test
    public void testHandleNoApis() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/no_apis.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        PythonClientCodegen pythonClientCodegen = new PythonClientCodegen();
        pythonClientCodegen.setOutputDir(output.getAbsolutePath());
        clientOptInput.config(pythonClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream().collect(Collectors.toMap(File::getPath, Function.identity()));

        File apiFile = files.get(Paths.get(output.getAbsolutePath(), "openapi_client", "api", "hello_example_api.py").toString());
        assertNull(apiFile);

        File setupFile = files.get(Paths.get(output.getAbsolutePath(), "setup.py").toString());
        assertNotNull(setupFile);
        assertFileContains(setupFile.toPath(), "setup(");
    }

    @Test(description = "outputs __init__.py with imports for exports")
    public void testInitFileImportsExports() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        PythonClientCodegen pythonClientCodegen = new PythonClientCodegen();
        pythonClientCodegen.setOutputDir(output.getAbsolutePath());
        clientOptInput.config(pythonClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream().collect(Collectors.toMap(File::getPath, Function.identity()));

        File initFile = files.get(Paths.get(output.getAbsolutePath(), "openapi_client", "__init__.py").toString());
        assertNotNull(initFile);
        Path initFilePath = initFile.toPath();

        // import apis into sdk package
        assertFileContains(initFilePath, "from openapi_client.api.pet_api import PetApi as PetApi");
        assertFileContains(initFilePath, "from openapi_client.api.store_api import StoreApi as StoreApi");
        assertFileContains(initFilePath, "from openapi_client.api.user_api import UserApi as UserApi");

        // import ApiClient
        assertFileContains(initFilePath, "from openapi_client.api_response import ApiResponse as ApiResponse");
        assertFileContains(initFilePath, "from openapi_client.api_client import ApiClient as ApiClient");
        assertFileContains(initFilePath, "from openapi_client.configuration import Configuration as Configuration");
        assertFileContains(initFilePath, "from openapi_client.exceptions import OpenApiException as OpenApiException");
        assertFileContains(initFilePath, "from openapi_client.exceptions import ApiTypeError as ApiTypeError");
        assertFileContains(initFilePath, "from openapi_client.exceptions import ApiValueError as ApiValueError");
        assertFileContains(initFilePath, "from openapi_client.exceptions import ApiKeyError as ApiKeyError");
        assertFileContains(initFilePath, "from openapi_client.exceptions import ApiAttributeError as ApiAttributeError");
        assertFileContains(initFilePath, "from openapi_client.exceptions import ApiException as ApiException");

        // import models into sdk package
        assertFileContains(initFilePath, "from openapi_client.models.api_response import ApiResponse as ApiResponse");
        assertFileContains(initFilePath, "from openapi_client.models.category import Category as Category");
        assertFileContains(initFilePath, "from openapi_client.models.order import Order as Order");
        assertFileContains(initFilePath, "from openapi_client.models.pet import Pet as Pet");
        assertFileContains(initFilePath, "from openapi_client.models.tag import Tag as Tag");
        assertFileContains(initFilePath, "from openapi_client.models.user import User as User");
    }

    @Test(description = "Verify default license format uses object notation when poetry1 is false")
    public void testLicenseFormatInPyprojectToml() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("python")
            .setInputSpec("src/test/resources/bugs/issue_21619.yaml")
            .setOutputDir(output.getAbsolutePath())
            .addAdditionalProperty("licenseInfo", "MIT");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output.getAbsolutePath(), "pyproject.toml"));
        // When poetry1=false (default), license should use object notation: { text = "MIT" }
        TestUtils.assertFileContains(Paths.get(output.getAbsolutePath(), "pyproject.toml"),
            "license = { text = \"MIT\" }");
    }

    @Test(description = "Verify poetry1 mode uses string notation for license")
    public void testPoetry1LicenseFormat() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("python")
            .setInputSpec("src/test/resources/bugs/issue_21619.yaml")
            .setOutputDir(output.getAbsolutePath())
            .addAdditionalProperty("licenseInfo", "Apache-2.0")
            .addAdditionalProperty("poetry1", true); // Enable legacy poetry1 mode

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path pyprojectPath = Paths.get(output.getAbsolutePath(), "pyproject.toml");
        TestUtils.assertFileExists(pyprojectPath);

        // In poetry1 mode, license should use simple string format: "Apache-2.0"
        TestUtils.assertFileContains(pyprojectPath, "license = \"Apache-2.0\"");

        // Verify it does NOT use the new object format
        TestUtils.assertFileNotContains(pyprojectPath, "license = { text = \"Apache-2.0\" }");
    }

    @Test(description = "Verify non-poetry1 mode uses object notation for license")
    public void testNonPoetry1LicenseFormat() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("python")
            .setInputSpec("src/test/resources/bugs/issue_21619.yaml")
            .setOutputDir(output.getAbsolutePath())
            .addAdditionalProperty("licenseInfo", "BSD-3-Clause")
            .addAdditionalProperty("poetry1", false); // Explicitly disable poetry1 mode

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path pyprojectPath = Paths.get(output.getAbsolutePath(), "pyproject.toml");
        TestUtils.assertFileExists(pyprojectPath);

        // In non-poetry1 mode, license should use object format: { text = "BSD-3-Clause" }
        TestUtils.assertFileContains(pyprojectPath, "license = { text = \"BSD-3-Clause\" }");

        // Verify it does NOT use the legacy string format
        TestUtils.assertFileNotContains(pyprojectPath, "license = \"BSD-3-Clause\"");
    }
}
