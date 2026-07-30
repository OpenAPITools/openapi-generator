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
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.languages.PythonClientCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
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
        Assert.assertEquals(
                codegen.additionalProperties().get(
                        PythonClientCodegen.USE_INDEPENDENT_IMPLICIT_CLIENTS),
                Boolean.FALSE);
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

    @Test(description = "test string enum default is quoted")
    public void testStringEnumDefaultIsQuoted() {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        StringSchema schema = new StringSchema();
        schema.setEnum(Arrays.asList("uploadTime", "duration", "fileSize"));
        schema.setDefault("uploadTime");
        String defaultValue = codegen.toDefaultValue(schema);
        Assert.assertEquals("'uploadTime'", defaultValue);
    }

    @Test(description = "test string enum default with special chars is quoted")
    public void testStringEnumDefaultFalseIsQuoted() {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        StringSchema schema = new StringSchema();
        schema.setEnum(Arrays.asList("true", "false"));
        schema.setDefault("false");
        String defaultValue = codegen.toDefaultValue(schema);
        Assert.assertEquals("'false'", defaultValue);
    }

    /**
     * Exposes protected helpers for unit tests without setAccessible() (forbiddenapis).
     */
    private static final class TestablePythonClientCodegen extends PythonClientCodegen {
        String unwrapPythonStringLiteralForTest(String value) {
            return unwrapPythonStringLiteral(value);
        }

        String getEnumDefaultValueForTest(String defaultValue, String dataType) {
            return getEnumDefaultValue(defaultValue, dataType);
        }
    }

    @Test(description = "multiline string enum default with apostrophe/backslash is quoted and unwraps for enum matching")
    public void testMultilineStringEnumDefaultUnwrapsEscapes() {
        final TestablePythonClientCodegen codegen = new TestablePythonClientCodegen();
        // Multiline forces triple quotes; apostrophe/backslash are escaped by formatPythonStringLiteral.
        final String multilineDefault = "line1\\it's\nline2";
        StringSchema schema = new StringSchema();
        schema.setEnum(Arrays.asList(multilineDefault, "other"));
        schema.setDefault(multilineDefault);

        String defaultValue = codegen.toDefaultValue(schema);
        Assert.assertEquals("'''line1\\\\it\\'s\nline2'''", defaultValue);

        // Triple-quoted unwrap must reverse the same escapes as the single-quoted path so
        // getEnumDefaultValue() can match against enumVars (see issue review on #23774).
        Assert.assertEquals(multilineDefault, codegen.unwrapPythonStringLiteralForTest(defaultValue));
        Assert.assertEquals(
                codegen.toEnumValue(multilineDefault, "str"),
                codegen.getEnumDefaultValueForTest(defaultValue, "str"));
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

    private static void addModelAttributeNameMappings(DefaultCodegen codegen) {
        codegen.nameMapping().put("continue", "_continue");
        codegen.nameMapping().put("schema", "schema");
        codegen.nameMapping().put("ordinary", "renamed");
        codegen.nameMapping().put("underscore", "_value");
        codegen.nameMapping().put("importCollision", "AliasChoices");
        codegen.nameMapping().put("dateTimeCollision", "datetime");
        codegen.nameMapping().put("kind", "_kind");
        codegen.nameMapping().put("model_dump", "var_model_dump");
        codegen.nameMapping().put("to_dict", "var_to_dict");
    }

    private static Throwable rootCause(Throwable throwable) {
        while (throwable.getCause() != null) {
            throwable = throwable.getCause();
        }
        return throwable;
    }

    private static Throwable expectModelNameMappingFailure(DefaultCodegen codegen) {
        RuntimeException exception = Assert.expectThrows(
                RuntimeException.class,
                () -> generateFiles(codegen,
                        "src/test/resources/3_0/python/model-attribute-alias.yaml"));
        return rootCause(exception);
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

    @Test(description = "UUID property with pattern should import field_validator")
    public void testUuidWithPatternImportsFieldValidator() throws IOException {
        final DefaultCodegen codegen = new PythonClientCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/bugs/issue_uuid_with_pattern.yaml");
        final Path p = Paths.get(outputPath + "openapi_client/models/uuid_with_pattern.py");

        assertFileExists(p);
        assertFileContains(p, "from pydantic import BaseModel, ConfigDict, field_validator");
    }

    @Test(description = "Verify default buildSystem uses setuptools")
    public void testDefaultBuildSystemSetuptools() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("python")
            .setInputSpec("src/test/resources/bugs/issue_21619.yaml")
            .setOutputDir(output.getAbsolutePath());

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path pyprojectPath = Paths.get(output.getAbsolutePath(), "pyproject.toml");
        TestUtils.assertFileExists(pyprojectPath);
        TestUtils.assertFileContains(pyprojectPath, "requires = [\"setuptools\"]");
        TestUtils.assertFileContains(pyprojectPath, "build-backend = \"setuptools.build_meta\"");
    }

    @Test(description = "Verify buildSystem=hatchling uses hatchling")
    public void testBuildSystemHatchling() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("python")
            .setInputSpec("src/test/resources/bugs/issue_21619.yaml")
            .setOutputDir(output.getAbsolutePath())
            .addAdditionalProperty("buildSystem", "hatchling");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path pyprojectPath = Paths.get(output.getAbsolutePath(), "pyproject.toml");
        TestUtils.assertFileExists(pyprojectPath);
        TestUtils.assertFileContains(pyprojectPath, "requires = [\"hatchling\"]");
        TestUtils.assertFileContains(pyprojectPath, "build-backend = \"hatchling.build\"");
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

    @Test
    public void testConstraintMapping() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("python")
                .setInputSpec("src/test/resources/3_0/unit_test_spec/format.yaml")
                .setOutputDir(output.getAbsolutePath());

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path filePath = Paths.get(output.getAbsolutePath(), "openapi_client/models/format_test.py");
        TestUtils.assertFileExists(filePath);

        TestUtils.assertFileContains(filePath, "integer: Optional[Annotated[int, Field(multiple_of=2, le=100, strict=True, ge=10)]]");
        TestUtils.assertFileContains(filePath, "number: Union[Annotated[float, Field(multiple_of=32.5, le=543.2, strict=True, ge=32.1)], Annotated[int, Field(le=543, strict=True, ge=33)]]");
        TestUtils.assertFileContains(filePath, "double: Optional[Union[Annotated[float, Field(le=123.4, strict=True, ge=67.8)], Annotated[int, Field(le=123, strict=True, ge=68)]]]");
        TestUtils.assertFileContains(filePath, "decimal: Optional[Annotated[Decimal, Field(multiple_of=0.1, lt=123.4, strict=True, gt=67.8)]]");
    }

    @Test
    public void testModelAttributeAlias() throws IOException {
        final DefaultCodegen codegen = new PythonClientCodegen();
        addModelAttributeNameMappings(codegen);
        final String outputPath = generateFiles(codegen,
                "src/test/resources/3_0/python/model-attribute-alias.yaml");
        final Path model = Paths.get(outputPath + "openapi_client/models/alias_model.py");
        final Path modelDoc = Paths.get(outputPath + "docs/AliasModel.md");
        final Path modelTest = Paths.get(outputPath + "test/test_alias_model.py");
        final Path validatorCollisionModel = Paths.get(
                outputPath + "openapi_client/models/validator_collision_model.py");
        final Path discriminatorBase = Paths.get(
                outputPath + "openapi_client/models/discriminator_alias_base.py");
        final Path discriminatorChild = Paths.get(
                outputPath + "openapi_client/models/discriminator_alias_child.py");
        final Path unmappedModel = Paths.get(
                outputPath + "openapi_client/models/base_alias_model.py");

        assertFileExists(model);
        assertFileContains(model,
                "alias=\"_continue\"",
                "validation_alias=AliasChoices(\"continue\", \"_continue\")",
                "serialization_alias=\"continue\"",
                "def _continue(self) -> _AliasModel_var_continue_public_type:",
                "def _continue(self, value: _AliasModel_var_continue_public_type) -> None:",
                "return self.var_continue",
                "@property  # type: ignore[override]",
                "def schema(self) -> _AliasModel_var_schema_public_type:",
                "@schema.setter",
                "def schema(self, value: _AliasModel_var_schema_public_type) -> None:  # pyright: ignore[reportIncompatibleMethodOverride]",
                "return self.var_schema",
                "renamed: Optional[StrictStr]",
                "validation_alias=AliasChoices(\"ordinary\", \"renamed\")",
                "serialization_alias=\"ordinary\"",
                "alias_choices: Optional[StrictStr]",
                "alias=\"AliasChoices\"",
                "validation_alias=AliasChoices(\"importCollision\", \"AliasChoices\")",
                "serialization_alias=\"importCollision\"",
                "def AliasChoices(self) -> _AliasModel_alias_choices_public_type:",
                "var_datetime: Optional[datetime]",
                "validation_alias=AliasChoices(\"dateTimeCollision\", \"datetime\")",
                "serialization_alias=\"dateTimeCollision\"",
                "def datetime(self) -> _AliasModel_var_datetime_public_type:",
                "camel_case: Optional[StrictStr]",
                "alias=\"camelCase\"",
                "validation_alias=AliasChoices(\"camelCase\", \"camel_case\")",
                "serialization_alias=\"camelCase\"",
                "value: Optional[StrictStr]",
                "def _value(self) -> _AliasModel_value_public_type:",
                "return self.value",
                "var_model_dump: Optional[StrictStr]",
                "validation_alias=AliasChoices(\"model_dump\", \"var_model_dump\")",
                "var_to_dict: Optional[StrictStr]",
                "validation_alias=AliasChoices(\"to_dict\", \"var_to_dict\")",
                "validate_by_name=True",
                "from collections.abc import Mapping as _Mapping",
                "ModelWrapValidatorHandler as _ModelWrapValidatorHandler",
                "model_validator as _model_validator",
                "cast as _cast",
                "def __preprocess_input_names(",
                "remove_hidden_storage_names: bool = True,",
                "@_model_validator(mode=\"wrap\")",
                "def __validate_input_names(",
                "handler: _ModelWrapValidatorHandler[Self]",
                "if not isinstance(obj, cls):",
                "obj = cls.__preprocess_input_names(obj)",
                "return handler(obj)",
                "if not isinstance(obj, _Mapping):",
                "\"continue\" in obj",
                "and \"_continue\" in obj",
                "cls.__name__,",
                "obj = cls.__preprocess_input_names(",
                "if \"continue\" not in obj and \"_continue\" in obj:",
                "obj[\"continue\"] = obj[\"_continue\"]",
                "obj.pop(\"_continue\", None)",
                "if \"camelCase\" not in obj and \"camel_case\" in obj:",
                "obj.pop(\"camel_case\", None)",
                "obj.pop(\"var_continue\", None)");
        TestUtils.assertFileNotContains(model,
                "    _continue: Optional[StrictStr]",
                "\"camelCase\" in obj\n            and \"camel_case\" in obj");
        assertFileContains(modelDoc, "**_continue**");
        assertFileContains(modelTest, "_continue =");
        TestUtils.assertFileNotContains(modelTest, "var_continue =");

        for (String wrapper : Arrays.asList("one_of_alias_model.py", "any_of_alias_model.py")) {
            final Path wrapperModel = Paths.get(outputPath + "openapi_client/models/" + wrapper);
            TestUtils.assertFileNotContains(
                    wrapperModel, "AliasChoices", "validation_alias", "serialization_alias",
                    "__preprocess_input_names", "_model_validator");
        }
        assertFileContains(validatorCollisionModel,
                "    status:",
                "def status_validate_enum(cls, value):",
                "    pattern:",
                "def pattern_validate_regular_expression(cls, value):",
                "    get_discriminator_value:");
        assertFileContains(discriminatorBase,
                "validation_alias=AliasChoices(\"kind\", \"_kind\")",
                "def _kind(self) -> _DiscriminatorAliasBase_kind_public_type:",
                "remove_hidden_storage_names=False");
        assertFileContains(discriminatorChild,
                "validation_alias=AliasChoices(\"childValue\", \"child_value\")",
                "if \"childValue\" not in obj and \"child_value\" in obj:",
                "obj.pop(\"child_value\", None)",
                "def __validate_input_names(",
                "validate_by_name=True");
        TestUtils.assertFileNotContains(discriminatorChild,
                "\"childValue\" in obj\n            and \"child_value\" in obj",
                "def _kind(");
        assertFileContains(unmappedModel, "validate_by_name=True");
        TestUtils.assertFileNotContains(unmappedModel,
                "AliasChoices", "_ModelWrapValidatorHandler", "__preprocess_input_names");
    }

    @Test
    public void testLegacyModelToDictRendering() throws IOException {
        final DefaultCodegen defaultCodegen = new PythonClientCodegen();
        addModelAttributeNameMappings(defaultCodegen);
        final String defaultOutputPath = generateFiles(defaultCodegen,
                "src/test/resources/3_0/python/legacy-model-dictionaries.yaml");
        final Path defaultModel = Paths.get(
                defaultOutputPath + "openapi_client/models/legacy_model.py");
        final Path defaultNestedModel = Paths.get(
                defaultOutputPath + "openapi_client/models/nested_model.py");
        final Path defaultWrapper = Paths.get(
                defaultOutputPath + "openapi_client/models/one_of_model.py");
        final Path defaultApi = Paths.get(
                defaultOutputPath + "openapi_client/api/default_api.py");
        final Path defaultApiClient = Paths.get(
                defaultOutputPath + "openapi_client/api_client.py");

        assertFileContains(defaultModel,
                "return json.dumps(to_jsonable_python(self.to_dict()))",
                "def to_dict(self) -> Dict[str, Any]:");
        assertFileContains(defaultWrapper,
                "def to_dict(self) -> Optional[Union[");
        assertFileContains(defaultApiClient,
                "if hasattr(obj, 'to_dict') and callable(getattr(obj, 'to_dict')):",
                "obj_dict = obj.to_dict()");
        TestUtils.assertFileNotContains(defaultModel,
                "_OPENAPI_GENERATOR_TO_DICT", "__openapi_generator_modern_projection",
                "serialize: bool", "openapi_types", "attribute_map",
                "extra=\"forbid\"", "def __repr__", "def __eq__");
        assertFileContains(defaultNestedModel, "alias=\"camelCase\"");
        TestUtils.assertFileNotContains(defaultNestedModel,
                "validation_alias=AliasChoices(\"camelCase\", \"camel_case\")");
        TestUtils.assertFileNotContains(defaultWrapper,
                "_OPENAPI_GENERATOR_TO_DICT", "__openapi_generator_modern_projection",
                "serialize: bool");
        TestUtils.assertFileNotContains(defaultApi,
                "async_req", "        _preload_content: bool = True",
                "return self.api_client.pool.apply_async(");
        TestUtils.assertFileNotContains(defaultApiClient, "_OPENAPI_GENERATOR_TO_DICT");

        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.additionalProperties().put(
                PythonClientCodegen.COMPATIBLE_WITH_PYTHON_LEGACY, true);
        addModelAttributeNameMappings(codegen);
        final String outputPath = generateFiles(codegen,
                "src/test/resources/3_0/python/legacy-model-dictionaries.yaml");
        final Path model = Paths.get(outputPath + "openapi_client/models/legacy_model.py");
        final Path nestedModel = Paths.get(
                outputPath + "openapi_client/models/nested_model.py");
        final Path api = Paths.get(
                outputPath + "openapi_client/api/default_api.py");
        final Path apiClient = Paths.get(outputPath + "openapi_client/api_client.py");

        assertFileContains(model,
                "def _get_openapi_to_dict(value: Any) -> Any:",
                "def _to_legacy_item(value: Any, serialize: bool) -> Any:",
                "def _to_legacy_value(value: Any, serialize: bool) -> Any:",
                "def _to_openapi_value(value: Any) -> Any:",
                "def to_dict(self, serialize: bool = False) -> Dict[str, Any]:",
                "_to_legacy_value(getattr(self, \"renamed\", None), serialize)",
                "def __openapi_generator_modern_projection(self) -> Dict[str, Any]:",
                "del __openapi_generator_modern_projection",
                "to_openapi_dict = _get_openapi_to_dict(self)",
                "return json.dumps(to_jsonable_python(to_openapi_dict(self)))",
                "return json.dumps(to_jsonable_python(self.to_dict()))",
                "openapi_types: ClassVar[Dict[str, str]] = {",
                "attribute_map: ClassVar[Dict[str, str]] = {",
                "\"_continue\": \"str\"",
                "\"_continue\": \"continue\"",
                "extra=\"forbid\"",
                "return pprint.pformat(self.to_dict())",
                "def __repr__(self) -> str:",
                "def __eq__(self, other: object) -> bool:",
                "def __ne__(self, other: object) -> bool:");
        TestUtils.assertFileNotContains(model,
                "_legacy_model_to_dict_impl: ClassVar", "def _to_openapi_dict(");
        assertFileContains(nestedModel,
                "camel_case: Optional[StrictStr]",
                "validation_alias=AliasChoices(\"camelCase\", \"camel_case\")",
                "serialization_alias=\"camelCase\"",
                "def __preprocess_input_names(");
        TestUtils.assertFileNotContains(nestedModel,
                "_ModelWrapValidatorHandler", "def __validate_input_names(");

        for (String wrapper : Arrays.asList("one_of_model.py", "any_of_model.py")) {
            final Path wrapperModel = Paths.get(outputPath + "openapi_client/models/" + wrapper);
            assertFileContains(wrapperModel,
                    "def to_dict(self, serialize: bool = False) -> Any:",
                    "def __openapi_generator_modern_projection(self) -> Any:",
                    "del __openapi_generator_modern_projection");
            TestUtils.assertFileNotContains(wrapperModel,
                    "_legacy_model_to_dict_impl: ClassVar", "def _to_openapi_dict(",
                    "openapi_types", "attribute_map", "extra=\"forbid\"",
                    "def __repr__", "def __eq__");
        }
        assertFileContains(apiClient,
                "def _get_openapi_to_dict(value: Any) -> Any:",
                "to_dict = getattr(obj, 'to_dict', None)",
                "to_openapi_dict = _get_openapi_to_dict(obj)",
                "if to_openapi_dict is not None:",
                "obj_dict = to_openapi_dict(obj)",
                "elif callable(to_dict):",
                "obj_dict = to_dict()",
                "pool_threads=1",
                "self._pool_lock = Lock()",
                "def _get_pool(self):",
                "def pool(self):",
                "ThreadPool(self.pool_threads)",
                "def _call_with_legacy_options(",
                "request: RequestSerialized",
                "response_types_map: Dict[str, Optional[str]]",
                "return self._get_pool().apply_async(call)",
                "response_data.getheaders()");
        assertFileContains(api,
                "async_req: Optional[bool] = None",
                "_preload_content: bool = True",
                ":param async_req: Whether to execute the request asynchronously.",
                ":param _preload_content: if False, the urllib3.HTTPResponse object will",
                "return self.api_client._call_with_legacy_options(",
                "_response_types_map,",
                "_request_timeout,",
                "True,",
                "False,");
        TestUtils.assertFileNotContains(apiClient, "Callable");
        TestUtils.assertFileNotContains(api,
                "def list_legacy_models_without_preload_content(");

        final PythonClientCodegen tornadoCodegen = new PythonClientCodegen();
        tornadoCodegen.setLibrary("tornado");
        tornadoCodegen.additionalProperties().put(
                PythonClientCodegen.COMPATIBLE_WITH_PYTHON_LEGACY, true);
        addModelAttributeNameMappings(tornadoCodegen);
        final String tornadoOutputPath = generateFiles(tornadoCodegen,
                "src/test/resources/3_0/python/legacy-model-dictionaries.yaml");
        final Path tornadoApi = Paths.get(
                tornadoOutputPath + "openapi_client/api/default_api.py");
        final Path tornadoApiClient = Paths.get(
                tornadoOutputPath + "openapi_client/api_client.py");
        TestUtils.assertFileNotContains(tornadoApi,
                "async_req", "        _preload_content: bool = True");
        TestUtils.assertFileNotContains(tornadoApiClient,
                "ThreadPool", "def _call_with_legacy_options(");
    }

    @Test
    public void testLegacyOperationControlParameterCollision() throws IOException {
        final DefaultCodegen defaultCodegen = new PythonClientCodegen();
        final String defaultOutputPath = generateFiles(defaultCodegen,
                "src/test/resources/3_0/python/legacy-operation-parameter-collision.yaml");
        final Path defaultApi = Paths.get(
                defaultOutputPath + "openapi_client/api/default_api.py");
        assertFileContains(defaultApi,
                "async_req: Optional[StrictStr] = None",
                "preload_content: Optional[StrictStr] = None");

        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.additionalProperties().put(
                PythonClientCodegen.COMPATIBLE_WITH_PYTHON_LEGACY, true);
        final String outputPath = generateFiles(codegen,
                "src/test/resources/3_0/python/legacy-operation-parameter-collision.yaml");
        final Path api = Paths.get(
                outputPath + "openapi_client/api/default_api.py");
        final Path model = Paths.get(
                outputPath + "openapi_client/models/collision_model.py");
        assertFileContains(api,
                "var_async_req: Optional[StrictStr] = None",
                "async_req: Optional[bool] = None",
                "preload_content: Optional[StrictStr] = None",
                "_preload_content: bool = True");
        TestUtils.assertFileNotContains(api,
                "\n        async_req: Optional[StrictStr] = None");
        assertFileContains(model, "async_req: Optional[StrictStr] = None");
        TestUtils.assertFileNotContains(model, "var_async_req:");
    }

    @Test
    public void testLegacyOperationMemberNames() throws IOException {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.additionalProperties().put(
                PythonClientCodegen.COMPATIBLE_WITH_PYTHON_LEGACY, true);
        codegen.additionalProperties().put(
                PythonClientCodegen.USE_INDEPENDENT_IMPLICIT_CLIENTS, true);
        final String outputPath = generateFiles(codegen,
                "src/test/resources/3_0/python/legacy-operation-member-names.yaml");
        final Path api = Paths.get(outputPath + "openapi_client/api/default_api.py");

        assertFileContains(api,
                "def call_close(",
                "def call_close_without_preload_content(");
        TestUtils.assertFileNotContains(api, "def call_close_2(");
    }

    @Test
    public void testLegacyModelMetadataRejectsSchemaFieldCollisions() {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.additionalProperties().put(
                PythonClientCodegen.COMPATIBLE_WITH_PYTHON_LEGACY, true);

        RuntimeException exception = Assert.expectThrows(
                RuntimeException.class,
                () -> generateFiles(
                        codegen,
                        "src/test/resources/3_0/python/legacy-model-metadata-collision.yaml"));

        Throwable cause = rootCause(exception);
        Assert.assertTrue(cause.getMessage().contains("openapi_types"), cause.toString());
    }

    @Test
    public void testLegacyModelToDictSupportsModelOnlyGeneration() throws IOException {
        final String oldModels = GlobalSettings.getProperty(CodegenConstants.MODELS);
        try {
            GlobalSettings.setProperty(CodegenConstants.MODELS, "LegacyModel");
            final PythonClientCodegen codegen = new PythonClientCodegen();
            codegen.setCompatibleWithPythonLegacy(true);
            addModelAttributeNameMappings(codegen);

            final String outputPath = generateFiles(codegen,
                    "src/test/resources/3_0/python/legacy-model-dictionaries.yaml");
            final Path model = Paths.get(
                    outputPath + "openapi_client/models/legacy_model.py");

            assertFileContains(model,
                    "def _get_openapi_to_dict(value: Any) -> Any:",
                    "def _to_legacy_item(value: Any, serialize: bool) -> Any:",
                    "def _to_legacy_value(value: Any, serialize: bool) -> Any:",
                    "def _to_openapi_value(value: Any) -> Any:");
            TestUtils.assertFileNotContains(model, "_legacy_model_dict import");
            Assert.assertFalse(Files.exists(Paths.get(
                    outputPath + "openapi_client/_legacy_model_dict.py")));
        } finally {
            if (oldModels == null) {
                GlobalSettings.clearProperty(CodegenConstants.MODELS);
            } else {
                GlobalSettings.setProperty(CodegenConstants.MODELS, oldModels);
            }
        }
    }

    @Test
    public void testModelPublicNameRejectsInvalidPythonNames() {
        for (String publicName : Arrays.asList(
                "class", "__private", "_iter", "model_dump", "property", "to_dict")) {
            final DefaultCodegen codegen = new PythonClientCodegen();
            codegen.nameMapping().put("value", publicName);

            Throwable cause = expectModelNameMappingFailure(codegen);

            Assert.assertTrue(cause.getMessage().contains(publicName), cause.toString());
        }
    }

    @Test
    public void testModelPublicNameRejectsConditionalGeneratedMemberNames() {
        for (Map.Entry<String, String> mapping : Map.of(
                "additionalValue", "additional_properties",
                "discriminatorHelper", "get_discriminator_value").entrySet()) {
            final DefaultCodegen codegen = new PythonClientCodegen();
            codegen.nameMapping().put(mapping.getKey(), mapping.getValue());

            Throwable cause = expectModelNameMappingFailure(codegen);

            Assert.assertTrue(cause.getMessage().contains(mapping.getValue()), cause.toString());
        }
    }

    @Test
    public void testModelPublicNameRejectsFieldCollisions() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.nameMapping().put("a", "_b");

        Throwable cause = expectModelNameMappingFailure(codegen);

        Assert.assertTrue(cause.getMessage().contains("_b"), cause.toString());
    }

    @Test
    public void testModelFieldRejectsGeneratedValidatorMemberCollision() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.nameMapping().put(
                "getDiscriminatorValue", "status_validate_enum");

        Throwable cause = expectModelNameMappingFailure(codegen);

        Assert.assertTrue(cause.getMessage().contains(
                "status_validate_enum"), cause.toString());
    }

    @Test
    public void testUnmappedModelFieldRejectsMappedValidatorMemberCollision() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.nameMapping().put("status", "mapped_status");

        Throwable cause = expectModelNameMappingFailure(codegen);

        Assert.assertTrue(cause.getMessage().contains(
                "mapped_status_validate_enum"), cause.toString());
    }

    @Test
    public void testUnmappedMemberCollisionsRemainOutsideNameMappingValidation()
            throws IOException {
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.nameMapping().put("ordinary", "renamed");

        final String outputPath = generateFiles(codegen,
                "src/test/resources/3_0/python/model-attribute-alias.yaml");
        final Path model = Paths.get(outputPath + "openapi_client/models/alias_model.py");

        // unmapped collisions are not rejected; they fall back to reserved-word
        // mangling and keep their wire name through the alias
        assertFileContains(model,
                "    var_model_dump: Optional[StrictStr] = Field(default=None, alias=\"model_dump\")",
                "    var_to_dict: Optional[StrictStr] = Field(default=None, alias=\"to_dict\")");
    }

    @Test
    public void testModelPublicNameRejectsStorageInputOverlap() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.nameMapping().put("crossFirst", "_cross_storage");
        codegen.nameMapping().put("cross_storage", "other_public");

        Throwable cause = expectModelNameMappingFailure(codegen);

        Assert.assertTrue(cause.getMessage().contains("cross_storage"), cause.toString());
    }

    @Test
    public void testModelPublicNameAllowsOwnWireStorageOverlap() throws IOException {
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.nameMapping().put("ordinary", "_ordinary");

        final String outputPath = generateFiles(codegen,
                "src/test/resources/3_0/python/model-attribute-alias.yaml");
        final Path model = Paths.get(outputPath + "openapi_client/models/alias_model.py");

        assertFileContains(model,
                "validation_alias=AliasChoices(\"ordinary\", \"_ordinary\")");
        TestUtils.assertFileNotContains(model, "obj.pop(\"ordinary\", None)");
    }

    @Test
    public void testModelPublicNameRejectsInheritedFieldCollisions() {
        for (Map.Entry<String, String> mapping : Map.of(
                "childValue", "parent_value",
                "parentValue", "child_value").entrySet()) {
            final DefaultCodegen codegen = new PythonClientCodegen();
            codegen.nameMapping().put(mapping.getKey(), mapping.getValue());

            Throwable cause = expectModelNameMappingFailure(codegen);

            Assert.assertTrue(cause.getMessage().contains(mapping.getValue()), cause.toString());
        }
    }

    @Test
    public void testModelPublicNameRejectsInheritedStorageInputOverlap() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.nameMapping().put("parentValue", "_child_value");

        Throwable cause = expectModelNameMappingFailure(codegen);

        Assert.assertTrue(cause.getMessage().contains("child_value"), cause.toString());
    }

    @Test
    public void testModelPublicNameRejectsMangledPrivateNames() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.nameMapping().put(
                "getDiscriminatorValue",
                "_ValidatorCollisionModel__properties");

        Throwable cause = expectModelNameMappingFailure(codegen);

        Assert.assertTrue(cause.getMessage().contains(
                "_ValidatorCollisionModel__properties"), cause.toString());
    }

    @Test
    public void testModelPublicNameRejectsTransitiveMangledPrivateNames() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.nameMapping().put(
                "value", "_MangledBase__properties");

        Throwable cause = expectModelNameMappingFailure(codegen);

        Assert.assertTrue(cause.getMessage().contains(
                "_MangledBase__properties"), cause.toString());
    }

    @Test
    public void testModelNameMappingDoesNotRenameParameters() {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.nameMapping().put("some-value", "model_value");

        Assert.assertEquals(codegen.toVarName("some-value"), "model_value");
        Assert.assertEquals(codegen.toParamName("some-value"), "some_value");

        codegen.parameterNameMapping().put("some-value", "parameter_value");
        Assert.assertEquals(codegen.toParamName("some-value"), "parameter_value");
    }

    @Test
    public void testGeneratedHelperAndPydanticNamesAreReserved() {
        final PythonClientCodegen codegen = new PythonClientCodegen();

        // names of the helper methods generated on every model
        Assert.assertEquals(codegen.toVarName("to_dict"), "var_to_dict");
        Assert.assertEquals(codegen.toVarName("from_dict"), "var_from_dict");
        Assert.assertEquals(codegen.toVarName("to_json"), "var_to_json");
        // pydantic BaseModel namespace
        Assert.assertEquals(codegen.toVarName("model_config"), "var_model_config");
        Assert.assertEquals(codegen.toVarName("model_fields"), "var_model_fields");
        Assert.assertEquals(codegen.toVarName("model_dump"), "var_model_dump");
        // similar-looking names must not be mangled
        Assert.assertEquals(codegen.toVarName("model_configuration"), "model_configuration");
    }

    @Test(dataProvider = "numberMappings")
    public void testMapNumberTo(String mapToNumber, String expectedType) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("python")
                .setInputSpec("src/test/resources/3_0/echo_api.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("mapNumberTo", mapToNumber);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path numberPropertiesOnlyPath = Paths.get(output.getAbsolutePath(), "openapi_client/models/number_properties_only.py");
        TestUtils.assertFileExists(numberPropertiesOnlyPath);

        TestUtils.assertFileContains(numberPropertiesOnlyPath, String.format(Locale.ROOT, "number: Optional[%s]", expectedType));
        TestUtils.assertFileContains(numberPropertiesOnlyPath, String.format(Locale.ROOT, "var_float: Optional[%s]", expectedType));
    }

    @DataProvider(name = "numberMappings")
    public Object[][] numberMappings() {
        return new Object[][] {
                { "float", "float" },
                { "Decimal", "Decimal" },
                { "StrictFloat", "StrictFloat" },
                { "Union[StrictFloat, StrictInt]", "Union[StrictFloat, StrictInt]" }
        };
    }

    @Test
    public void testIndependentImplicitClientLifecycleOperationNames()
            throws IOException {
        final PythonClientCodegen disabled = new PythonClientCodegen();
        disabled.processOpts();
        Assert.assertEquals(disabled.toOperationId("close"), "close");

        final PythonClientCodegen httpxSync = new PythonClientCodegen();
        httpxSync.setLibrary("httpx");
        httpxSync.additionalProperties().put(
                PythonClientCodegen.USE_INDEPENDENT_IMPLICIT_CLIENTS, true);
        httpxSync.additionalProperties().put(PythonClientCodegen.SUPPORT_HTTPX_SYNC, true);
        final String outputPath = generateFiles(httpxSync,
                "src/test/resources/3_0/python/independent-client-operation-names.yaml");
        final Path api = Paths.get(outputPath + "openapi_client/api/default_api.py");

        assertFileContains(api,
                "async def call_close_2(",
                "async def call_close_sync_2(");
        List<String> methodNames = Files.readAllLines(api).stream()
                .filter(line -> line.startsWith("    def ")
                        || line.startsWith("    async def "))
                .map(line -> line.substring(
                        line.indexOf("def ") + 4, line.indexOf('(')))
                .collect(Collectors.toList());
        Assert.assertEquals(
                methodNames.stream().distinct().count(),
                methodNames.size(),
                "generated API method names must be unique");
    }

    @Test
    public void testIndependentImplicitClients() throws IOException {
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.additionalProperties().put(
                PythonClientCodegen.USE_INDEPENDENT_IMPLICIT_CLIENTS, true);
        final String outputPath = generateFiles(codegen,
                "src/test/resources/3_0/generic.yaml");
        final Path configuration = Paths.get(outputPath + "openapi_client/configuration.py");
        final Path apiClient = Paths.get(outputPath + "openapi_client/api_client.py");
        final Path api = Paths.get(outputPath + "openapi_client/api/default_api.py");
        final Path rest = Paths.get(outputPath + "openapi_client/rest.py");

        assertFileContains(configuration,
                "cls._default = copy.deepcopy(default)",
                "if cls._default is not None:",
                "return copy.deepcopy(cls._default)",
                "return cls()",
                "if k == 'proxy_headers':",
                "copy_method = getattr(v, 'copy', None)",
                "if callable(copy_method):",
                "result.logger_file_handler = self.logger_file_handler");
        assertFileContains(apiClient,
                "configuration = Configuration.get_default_copy()",
                "def _get_default_or_new(cls):",
                "if cls._default is not None:",
                "return cls._default, False",
                "return cls(), True",
                "self.rest_client.close()");
        assertFileContains(api,
                "api_client, owns_api_client = ApiClient._get_default_or_new()",
                "owns_api_client = False",
                "self._owned_api_client: Optional[ApiClient] = (",
                "api_client if owns_api_client else None",
                "def close(self) -> None:",
                "owned_api_client = self._owned_api_client",
                "self._owned_api_client = None",
                "if owned_api_client is not None:",
                "owned_api_client.close()",
                "def __enter__(self):",
                "def __exit__(self, exc_type, exc_value, traceback):");
        assertFileContains(rest,
                "def close(self) -> None:",
                "self.pool_manager.clear()");

        final PythonClientCodegen asyncioCodegen = new PythonClientCodegen();
        asyncioCodegen.setLibrary("asyncio");
        asyncioCodegen.additionalProperties().put(
                PythonClientCodegen.USE_INDEPENDENT_IMPLICIT_CLIENTS, true);
        final String asyncioOutputPath = generateFiles(asyncioCodegen,
                "src/test/resources/3_0/generic.yaml");
        assertFileContains(
                Paths.get(asyncioOutputPath + "openapi_client/rest.py"),
                "if extra is not None and \"connector\" in extra",
                "if extra.get(\"connector\") is not None:",
                "kwargs[\"connector_owner\"] = False",
                "kwargs[\"connector_owner\"] = True");

        final PythonClientCodegen tornadoCodegen = new PythonClientCodegen();
        tornadoCodegen.setLibrary("tornado");
        tornadoCodegen.additionalProperties().put(
                PythonClientCodegen.USE_INDEPENDENT_IMPLICIT_CLIENTS, true);
        final String tornadoOutputPath = generateFiles(tornadoCodegen,
                "src/test/resources/3_0/generic.yaml");
        assertFileContains(
                Paths.get(tornadoOutputPath + "openapi_client/rest.py"),
                "httpclient.AsyncHTTPClient(force_instance=True)",
                "def close(self) -> None:",
                "self.pool_manager.close()");
    }

    @Test
    public void testIndependentImplicitClientsPreserveDefaultsWhenDisabled() throws IOException {
        final String outputPath = generateFiles(new PythonClientCodegen(),
                "src/test/resources/3_0/generic.yaml");
        final Path configuration = Paths.get(outputPath + "openapi_client/configuration.py");
        final Path apiClient = Paths.get(outputPath + "openapi_client/api_client.py");
        final Path api = Paths.get(outputPath + "openapi_client/api/default_api.py");
        final Path rest = Paths.get(outputPath + "openapi_client/rest.py");

        assertFileContains(configuration,
                "if k not in ('logger', 'logger_file_handler'):",
                "result.logger_file = self.logger_file");
        TestUtils.assertFileNotContains(configuration,
                "if k == 'proxy_headers':",
                "result.logger_file_handler = self.logger_file_handler");
        TestUtils.assertFileNotContains(apiClient,
                "configuration = Configuration.get_default_copy()",
                "def _get_default_or_new(cls):",
                "self.rest_client.close()");
        TestUtils.assertFileNotContains(api,
                "_owned_api_client",
                "def close(self) -> None:");
        TestUtils.assertFileNotContains(rest, "def close(self) -> None:");
    }
}
