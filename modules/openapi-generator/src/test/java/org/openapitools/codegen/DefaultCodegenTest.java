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

package org.openapitools.codegen;

import com.google.common.collect.Sets;
import com.samskivert.mustache.Mustache.Lambda;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.QueryParameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.templating.mustache.CamelCaseLambda;
import org.openapitools.codegen.templating.mustache.IndentedLambda;
import org.openapitools.codegen.templating.mustache.LowercaseLambda;
import org.openapitools.codegen.templating.mustache.TitlecaseLambda;
import org.openapitools.codegen.templating.mustache.UppercaseLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.SemVer;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import static org.testng.Assert.*;


public class DefaultCodegenTest {

    @Test
    public void testHasBodyParameter() {
        final Schema refSchema = new Schema<>().$ref("#/components/schemas/Pet");
        Operation pingOperation = new Operation()
                .responses(
                        new ApiResponses().addApiResponse("204", new ApiResponse()
                                .description("Ok response")));
        Operation createOperation = new Operation()
                .requestBody(new RequestBody()
                        .content(new Content().addMediaType("application/json",
                                new MediaType().schema(refSchema))))
                .responses(
                        new ApiResponses().addApiResponse("201", new ApiResponse()
                                .description("Created response")));
        OpenAPI openAPI = new OpenAPI();
        openAPI.setComponents(new Components());
        openAPI.getComponents().addSchemas("Pet", new ObjectSchema());

        final DefaultCodegen codegen = new DefaultCodegen();

        Assert.assertEquals(codegen.hasBodyParameter(openAPI, pingOperation), false);
        Assert.assertEquals(codegen.hasBodyParameter(openAPI, createOperation), true);
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void testParameterEmptyDescription() {
        DefaultCodegen codegen = new DefaultCodegen();

        codegen.fromRequestBody(null, new HashSet<>(), null);
    }

    @Test
    public void testGetConsumesInfoAndGetProducesInfo() throws Exception {
        final Schema refSchema = new Schema<>().$ref("#/components/schemas/Pet");
        OpenAPI openAPI = new OpenAPI();
        openAPI.setComponents(new Components());
        openAPI.getComponents().addSchemas("Pet", new ObjectSchema());
        openAPI.getComponents().addRequestBodies("MyRequestBody", new RequestBody()
                .content(new Content().addMediaType("application/json",
                        new MediaType().schema(refSchema))));
        openAPI.getComponents().addResponses("MyResponse", new ApiResponse()
                .description("Ok response")
                .content(new Content().addMediaType("application/xml",
                        new MediaType().schema(refSchema))));

        Operation createOperation = new Operation()
                .requestBody(new RequestBody()
                        .content(new Content()
                                .addMediaType("application/json", new MediaType().schema(refSchema))
                                .addMediaType("application/xml", new MediaType().schema(refSchema))
                        ))
                .responses(
                        new ApiResponses().addApiResponse("201", new ApiResponse()
                                .description("Created response")));
        Set<String> createConsumesInfo = DefaultCodegen.getConsumesInfo(openAPI, createOperation);
        Assert.assertEquals(createConsumesInfo.size(), 2);
        Assert.assertTrue(createConsumesInfo.contains("application/json"), "contains 'application/json'");
        Assert.assertTrue(createConsumesInfo.contains("application/xml"), "contains 'application/xml'");
        Set<String> createProducesInfo = DefaultCodegen.getProducesInfo(openAPI, createOperation);
        Assert.assertEquals(createProducesInfo.size(), 0);
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);
        CodegenOperation coCreate = codegen.fromOperation("somepath", "post", createOperation, null);
        Assert.assertTrue(coCreate.hasConsumes);
        Assert.assertEquals(coCreate.consumes.size(), 2);
        Assert.assertFalse(coCreate.hasProduces);

        Operation updateOperationWithRef = new Operation()
                .requestBody(new RequestBody().$ref("#/components/requestBodies/MyRequestBody"))
                .responses(new ApiResponses().addApiResponse("201", new ApiResponse().$ref("#/components/responses/MyResponse")));
        Set<String> updateConsumesInfo = DefaultCodegen.getConsumesInfo(openAPI, updateOperationWithRef);
        Assert.assertEquals(updateConsumesInfo.size(), 1);
        Assert.assertTrue(updateConsumesInfo.contains("application/json"), "contains 'application/json'");
        Set<String> updateProducesInfo = DefaultCodegen.getProducesInfo(openAPI, updateOperationWithRef);
        Assert.assertEquals(updateProducesInfo.size(), 1);
        Assert.assertTrue(updateProducesInfo.contains("application/xml"), "contains 'application/xml'");

        CodegenOperation coUpdate = codegen.fromOperation("somepath", "post", updateOperationWithRef, null);
        Assert.assertTrue(coUpdate.hasConsumes);
        Assert.assertEquals(coUpdate.consumes.size(), 1);
        Assert.assertEquals(coUpdate.consumes.get(0).get("mediaType"), "application/json");
        Assert.assertTrue(coUpdate.hasProduces);
        Assert.assertEquals(coUpdate.produces.size(), 1);
        Assert.assertEquals(coUpdate.produces.get(0).get("mediaType"), "application/xml");
    }

    @Test
    public void testGetProducesInfo() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/produces.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Operation textOperation = openAPI.getPaths().get("/ping/text").getGet();
        CodegenOperation coText = codegen.fromOperation("/ping/text", "get", textOperation, null);
        Assert.assertTrue(coText.hasProduces);
        Assert.assertEquals(coText.produces.size(), 1);
        Assert.assertEquals(coText.produces.get(0).get("mediaType"), "text/plain");

        Operation jsonOperation = openAPI.getPaths().get("/ping/json").getGet();
        CodegenOperation coJson = codegen.fromOperation("/ping/json", "get", jsonOperation, null);
        Assert.assertTrue(coJson.hasProduces);
        Assert.assertEquals(coJson.produces.size(), 1);
        Assert.assertEquals(coJson.produces.get(0).get("mediaType"), "application/json");

        Operation issue443Operation = openAPI.getPaths().get("/other/issue443").getGet();
        CodegenOperation coIssue443 = codegen.fromOperation("/other/issue443", "get", issue443Operation, null);
        Assert.assertTrue(coIssue443.hasProduces);
        Assert.assertEquals(coIssue443.produces.size(), 2);
        Assert.assertEquals(coIssue443.produces.get(0).get("mediaType"), "application/json");
        Assert.assertEquals(coIssue443.produces.get(0).get("hasMore"), "true");
        Assert.assertEquals(coIssue443.produces.get(1).get("mediaType"), "application/text");
        Assert.assertEquals(coIssue443.produces.get(1).get("hasMore"), null);
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testArraySchemaIsNotIncluedInAliases() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        Map<String, Schema> schemas = new HashMap<String, Schema>() {
            {
                put("ArraySchemaTest", new ArraySchema());
            }

        };

        Map<String, String> aliases = codegen.getAllAliases(schemas);

        Assert.assertEquals(aliases.size(), 0);
    }

    @Test
    public void testFormParameterHasDefaultValue() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Schema requestBodySchema = ModelUtils.getSchemaFromRequestBody(openAPI.getPaths().get("/fake").getGet().getRequestBody());
        CodegenParameter codegenParameter = codegen.fromFormProperty("enum_form_string", (Schema) requestBodySchema.getProperties().get("enum_form_string"), new HashSet<String>());

        Assert.assertEquals(codegenParameter.defaultValue, "-efg");
    }

    @Test
    public void testOriginalOpenApiDocumentVersion() {
        // Test with OAS 2.0 document.
        String location = "src/test/resources/2_0/python-client-experimental/petstore-with-fake-endpoints-models-for-testing.yaml";
        OpenAPI openAPI = TestUtils.parseFlattenSpec(location);
        SemVer version = ModelUtils.getOpenApiVersion(openAPI, location, null);
        Assert.assertEquals(version, new SemVer("2.0.0"));

        // Test with OAS 3.0 document.
        location = "src/test/resources/3_0/python-experimental/petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml";
        openAPI = TestUtils.parseFlattenSpec(location);
        version = ModelUtils.getOpenApiVersion(openAPI, location, null);
        Assert.assertEquals(version, new SemVer("3.0.0"));
    }

    @Test
    public void testAdditionalPropertiesV2Spec() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/additional-properties-for-testing.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setDisallowAdditionalPropertiesIfNotPresent(true);

        Schema schema = openAPI.getComponents().getSchemas().get("AdditionalPropertiesClass");
        Assert.assertEquals(schema.getAdditionalProperties(), null);

        Schema addProps = ModelUtils.getAdditionalProperties(openAPI, schema);
        // The petstore-with-fake-endpoints-models-for-testing.yaml does not set the
        // 'additionalProperties' keyword for this model, hence assert the value to be null.
        Assert.assertNull(addProps);
        CodegenModel cm = codegen.fromModel("AdditionalPropertiesClass", schema);
        // When the 'additionalProperties' keyword is not present, the model
        // should allow undeclared properties. However, due to bug
        // https://github.com/swagger-api/swagger-parser/issues/1369, the swagger
        // converter does not retain the value of the additionalProperties.

        Map<String, Schema> m = schema.getProperties();
        Schema child = m.get("map_string");
        // This property has the following inline schema.
        // additionalProperties:
        //   type: string
        Assert.assertNotNull(child);
        Assert.assertNotNull(child.getAdditionalProperties());

        child = m.get("map_with_additional_properties");
        // This property has the following inline schema.
        // additionalProperties: true
        Assert.assertNotNull(child);
        // It is unfortunate that child.getAdditionalProperties() returns null for a V2 schema.
        // We cannot differentiate between 'additionalProperties' not present and
        // additionalProperties: true.
        Assert.assertNull(child.getAdditionalProperties());
        addProps = ModelUtils.getAdditionalProperties(openAPI, child);
        Assert.assertNull(addProps);

        child = m.get("map_without_additional_properties");
        // This property has the following inline schema.
        // additionalProperties: false
        Assert.assertNotNull(child);
        // It is unfortunate that child.getAdditionalProperties() returns null for a V2 schema.
        // We cannot differentiate between 'additionalProperties' not present and
        // additionalProperties: false.
        Assert.assertNull(child.getAdditionalProperties());
        addProps = ModelUtils.getAdditionalProperties(openAPI, child);
        Assert.assertNull(addProps);
    }

    @Test
    public void testAdditionalPropertiesV3Spec() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/python-experimental/petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setDisallowAdditionalPropertiesIfNotPresent(false);
        codegen.setOpenAPI(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("AdditionalPropertiesClass");
        Assert.assertNull(schema.getAdditionalProperties());

        // When the 'additionalProperties' keyword is not present, the schema may be
        // extended with any undeclared properties.
        Schema addProps = ModelUtils.getAdditionalProperties(openAPI, schema);
        Assert.assertNotNull(addProps);
        Assert.assertTrue(addProps instanceof ObjectSchema);
        CodegenModel cm = codegen.fromModel("AdditionalPropertiesClass", schema);

        Map<String, Schema> m = schema.getProperties();
        Schema child = m.get("map_with_undeclared_properties_string");
        // This property has the following inline schema.
        // additionalProperties:
        //   type: string
        Assert.assertNotNull(child);
        Assert.assertNotNull(child.getAdditionalProperties());

        child = m.get("map_with_undeclared_properties_anytype_1");
        // This property does not use the additionalProperties keyword,
        // which means by default undeclared properties are allowed.
        Assert.assertNotNull(child);
        Assert.assertNull(child.getAdditionalProperties());
        addProps = ModelUtils.getAdditionalProperties(openAPI, child);
        Assert.assertNotNull(addProps);
        Assert.assertTrue(addProps instanceof ObjectSchema);

        child = m.get("map_with_undeclared_properties_anytype_2");
        // This property does not use the additionalProperties keyword,
        // which means by default undeclared properties are allowed.
        Assert.assertNotNull(child);
        Assert.assertNull(child.getAdditionalProperties());
        addProps = ModelUtils.getAdditionalProperties(openAPI, child);
        Assert.assertNotNull(addProps);
        Assert.assertTrue(addProps instanceof ObjectSchema);

        child = m.get("map_with_undeclared_properties_anytype_3");
        // This property has the following inline schema.
        // additionalProperties: true
        Assert.assertNotNull(child);
        // Unlike the V2 spec, in V3 we CAN differentiate between 'additionalProperties' not present and
        // additionalProperties: true.
        Assert.assertNotNull(child.getAdditionalProperties());
        Assert.assertEquals(child.getAdditionalProperties(), Boolean.TRUE);
        addProps = ModelUtils.getAdditionalProperties(openAPI, child);
        Assert.assertNotNull(addProps);
        Assert.assertTrue(addProps instanceof ObjectSchema);

        child = m.get("empty_map");
        // This property has the following inline schema.
        // additionalProperties: false
        Assert.assertNotNull(child);
        // Unlike the V2 spec, in V3 we CAN differentiate between 'additionalProperties' not present and
        // additionalProperties: false.
        Assert.assertNotNull(child.getAdditionalProperties());
        Assert.assertEquals(child.getAdditionalProperties(), Boolean.FALSE);
        addProps = ModelUtils.getAdditionalProperties(openAPI, child);
        Assert.assertNull(addProps);
    }

    @Test
    public void testAdditionalPropertiesV3SpecLegacy() {
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setDisallowAdditionalPropertiesIfNotPresent(true);
        codegen.setOpenAPI(openAPI);

        Schema schema = openAPI.getComponents().getSchemas().get("AdditionalPropertiesClass");
        Assert.assertNull(schema.getAdditionalProperties());

        // As per OAS spec, when the 'additionalProperties' keyword is not present, the schema may be
        // extended with any undeclared properties.
        // However, in legacy 'additionalProperties' mode, this is interpreted as
        // 'no additional properties are allowed'.
        Schema addProps = ModelUtils.getAdditionalProperties(openAPI, schema);
        Assert.assertNull(addProps);
    }

    @Test
    public void testEnsureNoDuplicateProduces() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/two-responses.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Operation operation = openAPI.getPaths().get("/test").getGet();
        CodegenOperation co = codegen.fromOperation("/test", "get", operation, null);

        Assert.assertEquals(co.produces.size(), 1);
        Assert.assertEquals(co.produces.get(0).get("mediaType"), "application/json");
    }

    @Test
    public void testConsistentParameterNameAfterUniquenessRename() throws Exception {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        Operation operation = new Operation()
                .operationId("opId")
                .addParametersItem(new QueryParameter().name("myparam").schema(new StringSchema()))
                .addParametersItem(new QueryParameter().name("myparam").schema(new StringSchema()))
                .responses(new ApiResponses().addApiResponse("200", new ApiResponse().description("OK")));

        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);
        CodegenOperation co = codegen.fromOperation("/some/path", "get", operation, null);
        Assert.assertEquals(co.path, "/some/path");
        Assert.assertEquals(co.allParams.size(), 2);
        List<String> allParamsNames = co.allParams.stream().map(p -> p.paramName).collect(Collectors.toList());
        Assert.assertTrue(allParamsNames.contains("myparam"));
        Assert.assertTrue(allParamsNames.contains("myparam2"));
        List<String> queryParamsNames = co.queryParams.stream().map(p -> p.paramName).collect(Collectors.toList());
        Assert.assertTrue(queryParamsNames.contains("myparam"));
        Assert.assertTrue(queryParamsNames.contains("myparam2"));
    }

    @Test
    public void testGetSchemaTypeWithComposedSchemaWithOneOf() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/composed-oneof.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/state").getPost();
        Schema schema = ModelUtils.getSchemaFromRequestBody(operation.getRequestBody());
        String type = codegen.getSchemaType(schema);

        Assert.assertNotNull(type);
        Assert.assertEquals(type, "oneOf<ObjA,ObjB>");
    }

    @Test
    public void testComposedSchemaOneOfWithProperties() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneOf.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();

        final Schema schema = openAPI.getComponents().getSchemas().get("fruit");
        codegen.setOpenAPI(openAPI);
        CodegenModel fruit = codegen.fromModel("Fruit", schema);

        Set<String> oneOf = new TreeSet<String>();
        oneOf.add("Apple");
        oneOf.add("Banana");
        Assert.assertEquals(fruit.oneOf, oneOf);
        Assert.assertEquals(fruit.optionalVars.size(), 3);
        Assert.assertEquals(fruit.vars.size(), 3);
        // make sure that fruit has the property color
        boolean colorSeen = false;
        for (CodegenProperty cp : fruit.vars) {
            if (cp.name.equals("color")) {
                colorSeen = true;
                break;
            }
        }
        Assert.assertTrue(colorSeen);
        colorSeen = false;
        for (CodegenProperty cp : fruit.optionalVars) {
            if (cp.name.equals("color")) {
                colorSeen = true;
                break;
            }
        }
        Assert.assertTrue(colorSeen);
    }


    @Test
    public void testEscapeText() {
        final DefaultCodegen codegen = new DefaultCodegen();

        Assert.assertEquals(codegen.escapeText("\n"), " ");
        Assert.assertEquals(codegen.escapeText("\r"), " ");
        Assert.assertEquals(codegen.escapeText("\t"), " ");
        Assert.assertEquals(codegen.escapeText("\\"), "\\\\");
        Assert.assertEquals(codegen.escapeText("\""), "\\\"");
        Assert.assertEquals(codegen.escapeText("\\/"), "/");
    }

    @Test
    public void testEscapeTextWhileAllowingNewLines() {
        final DefaultCodegen codegen = new DefaultCodegen();

        // allow new lines
        Assert.assertEquals(codegen.escapeTextWhileAllowingNewLines("\n"), "\n");
        Assert.assertEquals(codegen.escapeTextWhileAllowingNewLines("\r"), "\r");

        // escape other special characters
        Assert.assertEquals(codegen.escapeTextWhileAllowingNewLines("\t"), " ");
        Assert.assertEquals(codegen.escapeTextWhileAllowingNewLines("\\"), "\\\\");
        Assert.assertEquals(codegen.escapeTextWhileAllowingNewLines("\""), "\\\"");
        Assert.assertEquals(codegen.escapeTextWhileAllowingNewLines("\\/"), "/");
    }

    @Test
    public void updateCodegenPropertyEnum() {
        final DefaultCodegen codegen = new DefaultCodegen();
        CodegenProperty array = codegenPropertyWithArrayOfIntegerValues();

        codegen.updateCodegenPropertyEnum(array);

        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) array.getItems().getAllowableValues().get("enumVars");
        Assert.assertNotNull(enumVars);
        Map<String, Object> testedEnumVar = enumVars.get(0);
        Assert.assertNotNull(testedEnumVar);
        Assert.assertEquals(testedEnumVar.getOrDefault("name", ""), "_1");
        Assert.assertEquals(testedEnumVar.getOrDefault("value", ""), "\"1\"");
        Assert.assertEquals(testedEnumVar.getOrDefault("isString", ""), false);
    }

    @Test
    public void updateCodegenPropertyEnumWithExtention() {
        {
            CodegenProperty enumProperty = codegenPropertyWithXEnumVarName(Arrays.asList("dog", "cat"), Arrays.asList("DOGVAR", "CATVAR"));
            (new DefaultCodegen()).updateCodegenPropertyEnum(enumProperty);
            List<Map<String, Object>> enumVars = (List<Map<String, Object>>) enumProperty.getAllowableValues().get("enumVars");
            Assert.assertNotNull(enumVars);
            Assert.assertNotNull(enumVars.get(0));
            Assert.assertEquals(enumVars.get(0).getOrDefault("name", ""), "DOGVAR");
            Assert.assertEquals(enumVars.get(0).getOrDefault("value", ""), "\"dog\"");
            Assert.assertNotNull(enumVars.get(1));
            Assert.assertEquals(enumVars.get(1).getOrDefault("name", ""), "CATVAR");
            Assert.assertEquals(enumVars.get(1).getOrDefault("value", ""), "\"cat\"");
        }
        {
            CodegenProperty enumProperty = codegenPropertyWithXEnumVarName(Arrays.asList("1", "2"), Arrays.asList("ONE", "TWO"));
            (new DefaultCodegen()).updateCodegenPropertyEnum(enumProperty);
            List<Map<String, Object>> enumVars = (List<Map<String, Object>>) enumProperty.getAllowableValues().get("enumVars");
            Assert.assertEquals(enumVars.get(0).getOrDefault("name", ""), "ONE");
            Assert.assertEquals(enumVars.get(0).getOrDefault("value", ""), "\"1\"");
            Assert.assertEquals(enumVars.get(1).getOrDefault("name", ""), "TWO");
            Assert.assertEquals(enumVars.get(1).getOrDefault("value", ""), "\"2\"");
        }
        {
            CodegenProperty enumProperty = codegenPropertyWithXEnumVarName(Arrays.asList("a", "b", "c", "d"), Arrays.asList("FOO", "BAR"));
            (new DefaultCodegen()).updateCodegenPropertyEnum(enumProperty);
            List<Map<String, Object>> enumVars = (List<Map<String, Object>>) enumProperty.getAllowableValues().get("enumVars");
            Assert.assertEquals(enumVars.get(0).getOrDefault("name", ""), "FOO");
            Assert.assertEquals(enumVars.get(1).getOrDefault("name", ""), "BAR");
            Assert.assertEquals(enumVars.get(2).getOrDefault("name", ""), "C");
            Assert.assertEquals(enumVars.get(3).getOrDefault("name", ""), "D");
        }
        {
            CodegenProperty enumProperty = codegenPropertyWithXEnumVarName(Arrays.asList("a", "b"), Arrays.asList("FOO", "BAR", "BAZ"));
            (new DefaultCodegen()).updateCodegenPropertyEnum(enumProperty);
            List<Map<String, Object>> enumVars = (List<Map<String, Object>>) enumProperty.getAllowableValues().get("enumVars");
            Assert.assertEquals(enumVars.get(0).getOrDefault("name", ""), "FOO");
            Assert.assertEquals(enumVars.get(1).getOrDefault("name", ""), "BAR");
            Assert.assertEquals(enumVars.size(), 2);
        }
    }

    @Test
    public void updateCodegenPropertyEnumWithPrefixRemoved() {
        final DefaultCodegen codegen = new DefaultCodegen();
        CodegenProperty enumProperty = codegenProperty(Arrays.asList("animal_dog", "animal_cat"));

        codegen.updateCodegenPropertyEnum(enumProperty);

        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) enumProperty.getItems().getAllowableValues().get("enumVars");
        Assert.assertNotNull(enumVars);
        Assert.assertNotNull(enumVars.get(0));
        Assert.assertEquals(enumVars.get(0).getOrDefault("name", ""), "DOG");
        Assert.assertEquals(enumVars.get(0).getOrDefault("value", ""), "\"animal_dog\"");
        Assert.assertNotNull(enumVars.get(1));
        Assert.assertEquals(enumVars.get(1).getOrDefault("name", ""), "CAT");
        Assert.assertEquals(enumVars.get(1).getOrDefault("value", ""), "\"animal_cat\"");
    }

    @Test
    public void updateCodegenPropertyEnumWithoutPrefixRemoved() {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setRemoveEnumValuePrefix(false);

        CodegenProperty enumProperty = codegenProperty(Arrays.asList("animal_dog", "animal_cat"));

        codegen.updateCodegenPropertyEnum(enumProperty);

        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) enumProperty.getItems().getAllowableValues().get("enumVars");
        Assert.assertNotNull(enumVars);
        Assert.assertNotNull(enumVars.get(0));
        Assert.assertEquals(enumVars.get(0).getOrDefault("name", ""), "ANIMAL_DOG");
        Assert.assertEquals(enumVars.get(0).getOrDefault("value", ""), "\"animal_dog\"");
        Assert.assertNotNull(enumVars.get(1));
        Assert.assertEquals(enumVars.get(1).getOrDefault("name", ""), "ANIMAL_CAT");
        Assert.assertEquals(enumVars.get(1).getOrDefault("value", ""), "\"animal_cat\"");
    }

    @Test
    public void postProcessModelsEnumWithPrefixRemoved() {
        final DefaultCodegen codegen = new DefaultCodegen();
        Map<String, Object> objs = codegenModel(Arrays.asList("animal_dog", "animal_cat"));
        CodegenModel cm = (CodegenModel) ((Map<String, Object>) ((List<Object>) objs.get("models")).get(0)).get("model");

        codegen.postProcessModelsEnum(objs);

        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) cm.getAllowableValues().get("enumVars");
        Assert.assertNotNull(enumVars);
        Assert.assertNotNull(enumVars.get(0));
        Assert.assertEquals(enumVars.get(0).getOrDefault("name", ""), "DOG");
        Assert.assertEquals(enumVars.get(0).getOrDefault("value", ""), "\"animal_dog\"");
        Assert.assertNotNull(enumVars.get(1));
        Assert.assertEquals(enumVars.get(1).getOrDefault("name", ""), "CAT");
        Assert.assertEquals(enumVars.get(1).getOrDefault("value", ""), "\"animal_cat\"");
    }

    @Test
    public void postProcessModelsEnumWithoutPrefixRemoved() {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setRemoveEnumValuePrefix(false);
        Map<String, Object> objs = codegenModel(Arrays.asList("animal_dog", "animal_cat"));
        CodegenModel cm = (CodegenModel) ((Map<String, Object>) ((List<Object>) objs.get("models")).get(0)).get("model");

        codegen.postProcessModelsEnum(objs);

        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) cm.getAllowableValues().get("enumVars");
        Assert.assertNotNull(enumVars);
        Assert.assertNotNull(enumVars.get(0));
        Assert.assertEquals(enumVars.get(0).getOrDefault("name", ""), "ANIMAL_DOG");
        Assert.assertEquals(enumVars.get(0).getOrDefault("value", ""), "\"animal_dog\"");
        Assert.assertNotNull(enumVars.get(1));
        Assert.assertEquals(enumVars.get(1).getOrDefault("name", ""), "ANIMAL_CAT");
        Assert.assertEquals(enumVars.get(1).getOrDefault("value", ""), "\"animal_cat\"");
    }

    @Test
    public void postProcessModelsEnumWithExtention() {
        final DefaultCodegen codegen = new DefaultCodegen();
        Map<String, Object> objs = codegenModelWithXEnumVarName();
        CodegenModel cm = (CodegenModel) ((Map<String, Object>) ((List<Object>) objs.get("models")).get(0)).get("model");

        codegen.postProcessModelsEnum(objs);

        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) cm.getAllowableValues().get("enumVars");
        Assert.assertNotNull(enumVars);
        Assert.assertNotNull(enumVars.get(0));
        Assert.assertEquals(enumVars.get(0).getOrDefault("name", ""), "DOGVAR");
        Assert.assertEquals(enumVars.get(0).getOrDefault("value", ""), "\"dog\"");
        Assert.assertEquals(enumVars.get(0).getOrDefault("enumDescription", ""), "This is a dog");
        Assert.assertNotNull(enumVars.get(1));
        Assert.assertEquals(enumVars.get(1).getOrDefault("name", ""), "CATVAR");
        Assert.assertEquals(enumVars.get(1).getOrDefault("value", ""), "\"cat\"");
        Assert.assertEquals(enumVars.get(1).getOrDefault("enumDescription", ""), "This is a cat");
    }

    @Test
    public void testExample1() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/examples.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/example1/singular").getGet();
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter, operation.getParameters().get(0));

        Assert.assertEquals(codegenParameter.example, "example1 value");

        Operation operation2 = openAPI.getPaths().get("/example1/plural").getGet();
        CodegenParameter codegenParameter2 = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter2, operation2.getParameters().get(0));

        Assert.assertEquals(codegenParameter2.example, "An example1 value");
    }

    @Test
    public void testExample2() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/examples.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/example2/singular").getGet();
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter, operation.getParameters().get(0));

        Assert.assertEquals(codegenParameter.example, "example2 value");
    }

    @Test
    public void testExample3() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/examples.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/example3/singular").getGet();
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter, operation.getParameters().get(0));

        Assert.assertEquals(codegenParameter.example, "example3: parameter value");

        Operation operation2 = openAPI.getPaths().get("/example3/plural").getGet();
        CodegenParameter codegenParameter2 = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter2, operation2.getParameters().get(0));

        Assert.assertEquals(codegenParameter2.example, "example3: parameter value");
    }

    @Test
    public void testExample4() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/examples.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/example4/singular").getPost();
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter, operation.getRequestBody());

        Assert.assertEquals(codegenParameter.example, "example4 value");

        Operation operation2 = openAPI.getPaths().get("/example4/plural").getPost();
        CodegenParameter codegenParameter2 = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter2, operation2.getRequestBody());

        Assert.assertEquals(codegenParameter2.example, "An example4 value");
    }

    @Test
    public void testDiscriminator() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        DefaultCodegen codegen = new DefaultCodegen();

        Schema animal = openAPI.getComponents().getSchemas().get("Animal");
        codegen.setOpenAPI(openAPI);
        CodegenModel animalModel = codegen.fromModel("Animal", animal);
        CodegenDiscriminator discriminator = animalModel.getDiscriminator();
        CodegenDiscriminator test = new CodegenDiscriminator();
        test.setPropertyName("className");
        test.setPropertyBaseName("className");
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("Dog", "Dog"));
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("Cat", "Cat"));
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("BigCat", "BigCat"));
        Assert.assertEquals(discriminator, test);
    }

    @Test
    public void testDiscriminatorWithCustomMapping() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setOpenAPI(openAPI);

        String path = "/person/display/{personId}";
        Operation operation = openAPI.getPaths().get(path).getGet();
        CodegenOperation codegenOperation = codegen.fromOperation(path, "GET", operation, null);
        verifyPersonDiscriminator(codegenOperation.discriminator);

        Schema person = openAPI.getComponents().getSchemas().get("Person");
        codegen.setOpenAPI(openAPI);
        CodegenModel personModel = codegen.fromModel("Person", person);
        verifyPersonDiscriminator(personModel.discriminator);
    }

    @Test
    public void testParentName() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf.yaml");
        DefaultCodegen codegen = new DefaultCodegen();

        Schema child = openAPI.getComponents().getSchemas().get("Child");
        codegen.setOpenAPI(openAPI);
        CodegenModel childModel = codegen.fromModel("Child", child);
        Assert.assertEquals(childModel.parentSchema, "Person");
    }

    @Test
    public void testAllOfRequired() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf-required.yaml");
        DefaultCodegen codegen = new DefaultCodegen();

        Schema child = openAPI.getComponents().getSchemas().get("clubForCreation");
        codegen.setOpenAPI(openAPI);
        CodegenModel childModel = codegen.fromModel("clubForCreation", child);
        Assert.assertEquals(getRequiredVars(childModel), Collections.singletonList("name"));
    }

    @Test
    public void testAllOfSingleAndDoubleRefWithOwnPropsNoDiscriminator() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf_composition.yaml");
        final DefaultCodegen codegen = new CodegenWithMultipleInheritance();

        codegen.setOpenAPI(openAPI);

        // to test allOf with double refs
        Schema supermanSchema = openAPI.getComponents().getSchemas().get("SuperMan");
        CodegenModel supermanModel = codegen.fromModel("SuperMan", supermanSchema);
        Assert.assertEquals(supermanModel.parent, null);
        Assert.assertEquals(supermanModel.allParents, null);

        // to test allOf with single ref
        Schema superboySchema = openAPI.getComponents().getSchemas().get("SuperBoy");
        CodegenModel superboyModel = codegen.fromModel("SuperBoy", superboySchema);
        Assert.assertEquals(superboyModel.parent, null);
        Assert.assertEquals(superboyModel.allParents, null);

        // to test allOf with single ref and no "type: object" in the (last) inline schema
        Schema superbabySchema = openAPI.getComponents().getSchemas().get("SuperBaby");
        CodegenModel superbabyModel = codegen.fromModel("SuperBaby", superbabySchema);
        Assert.assertEquals(superbabyModel.parent, null);
        Assert.assertEquals(superbabyModel.allParents, null);
    }

    @Test
    public void testAllParents() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOfMappingDuplicatedProperties.yaml");
        final DefaultCodegen codegen = new CodegenWithMultipleInheritance();

        codegen.setOpenAPI(openAPI);

        Schema adultSchema = openAPI.getComponents().getSchemas().get("Adult");
        CodegenModel adultModel = codegen.fromModel("Adult", adultSchema);
        Assert.assertEquals(adultModel.parent, "Person");
        Assert.assertEquals(adultModel.allParents, Collections.singletonList("Person"));
    }

    @Test
    public void testComposedSchemaAllOfDiscriminatorMap() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf_composition_discriminator.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setLegacyDiscriminatorBehavior(false);
        Schema sc;
        String modelName;

        String propertyName = "petType";
        String propertyBaseName = propertyName;
        CodegenDiscriminator emptyMapDisc = new CodegenDiscriminator();
        emptyMapDisc.setPropertyName(propertyName);
        emptyMapDisc.setPropertyBaseName(propertyBaseName);

        // all leaf Schemas have discriminators with PropertyName/BaseName + empty discriminator maps
        List<String> leafModelNames = Arrays.asList("Cat", "Dog", "Lizard", "Snake");
        for (String leafModelName: leafModelNames) {
            Schema leafSc = openAPI.getComponents().getSchemas().get(leafModelName);
            CodegenModel leafCm = codegen.fromModel(leafModelName, leafSc);
            Assert.assertEquals(leafCm.discriminator, emptyMapDisc);
        }

        // the Pet discriminator map contains all animals + Reptile (children + grandchildren)
        CodegenDiscriminator petDisc = new CodegenDiscriminator();
        petDisc.setPropertyName(propertyName);
        petDisc.setPropertyBaseName(propertyBaseName);
        java.util.LinkedHashSet hs = new LinkedHashSet<>();
        for (String leafModelName: leafModelNames) {
            hs.add(new CodegenDiscriminator.MappedModel(leafModelName, codegen.toModelName(leafModelName)));
        }
        hs.add(new CodegenDiscriminator.MappedModel("Reptile", codegen.toModelName("Reptile")));
        petDisc.setMappedModels(hs);
        modelName = "Pet";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        CodegenModel pet = codegen.fromModel(modelName, sc);
        Assert.assertEquals(pet.discriminator, petDisc);

        // the Reptile discriminator contains both reptiles
        List<String> reptileModelNames = Arrays.asList("Lizard", "Snake");
        CodegenDiscriminator reptileDisc = new CodegenDiscriminator();
        reptileDisc.setPropertyName(propertyName);
        reptileDisc.setPropertyBaseName(propertyBaseName);
        hs.clear();
        for (String reptileModelName: reptileModelNames) {
            hs.add(new CodegenDiscriminator.MappedModel(reptileModelName, codegen.toModelName(reptileModelName)));
        }
        reptileDisc.setMappedModels(hs);
        modelName = "Reptile";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        CodegenModel reptile = codegen.fromModel(modelName, sc);
        Assert.assertEquals(reptile.discriminator, reptileDisc);

        // the MyPets discriminator contains Cat and Lizard
        List<String> myPetNames = Arrays.asList("Cat", "Lizard");
        CodegenDiscriminator myPetDisc = new CodegenDiscriminator();
        myPetDisc.setPropertyName(propertyName);
        myPetDisc.setPropertyBaseName(propertyBaseName);
        hs.clear();
        for (String myPetName: myPetNames) {
            hs.add(new CodegenDiscriminator.MappedModel(myPetName, codegen.toModelName(myPetName)));
        }
        myPetDisc.setMappedModels(hs);
        modelName = "MyPets";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        CodegenModel myPets = codegen.fromModel(modelName, sc);
        Assert.assertEquals(myPets.discriminator, myPetDisc);

        // the MyPetsNoDisc discriminator is created because all oneOf classes have the same discriminator
        modelName = "MyPetsNoDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        CodegenModel myPetsNoDisc = codegen.fromModel(modelName, sc);
        Assert.assertEquals(myPetsNoDisc.discriminator, myPetDisc);

        CodegenModel cm;

        // the mapping in b is in A
        modelName = "A";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs.clear();
        hs.add(new CodegenDiscriminator.MappedModel("b", codegen.toModelName("B")));
        hs.add(new CodegenDiscriminator.MappedModel("B", codegen.toModelName("B")));
        hs.add(new CodegenDiscriminator.MappedModel("C", codegen.toModelName("C")));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // the mapping in b is in B
        modelName = "B";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs.clear();
        hs.add(new CodegenDiscriminator.MappedModel("b", codegen.toModelName("B")));
        hs.add(new CodegenDiscriminator.MappedModel("C", codegen.toModelName("C")));

        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // the mapping in b is in C
        modelName = "C";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs.clear();
        hs.add(new CodegenDiscriminator.MappedModel("b", codegen.toModelName("B")));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);
    }

    @Test
    public void testComposedSchemaAllOfDiscriminatorMapLegacy() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf_composition_discriminator.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        // codegen.discriminatorExplicitMappingVerbose remains false in the legacy use case
        codegen.setOpenAPI(openAPI);
        Schema sc;
        String modelName;

        String propertyName = "petType";
        String propertyBaseName = propertyName;
        CodegenDiscriminator emptyMapDisc = new CodegenDiscriminator();
        emptyMapDisc.setPropertyName(propertyName);
        emptyMapDisc.setPropertyBaseName(propertyBaseName);

        // all leaf Schemas have discriminators with PropertyName/BaseName + empty discriminator maps
        List<String> leafModelNames = Arrays.asList("Cat", "Dog", "Lizard", "Snake");
        for (String leafModelName: leafModelNames) {
            Schema leafSc = openAPI.getComponents().getSchemas().get(leafModelName);
            CodegenModel leafCm = codegen.fromModel(leafModelName, leafSc);
            Assert.assertEquals(leafCm.discriminator, null);
        }

        // the Pet discriminator map contains all animals + Reptile (children + grandchildren)
        CodegenDiscriminator petDisc = new CodegenDiscriminator();
        petDisc.setPropertyName(propertyName);
        petDisc.setPropertyBaseName(propertyBaseName);
        java.util.LinkedHashSet hs = new LinkedHashSet<>();
        for (String leafModelName: leafModelNames) {
            hs.add(new CodegenDiscriminator.MappedModel(leafModelName, codegen.toModelName(leafModelName)));
        }
        hs.add(new CodegenDiscriminator.MappedModel("Reptile", codegen.toModelName("Reptile")));
        petDisc.setMappedModels(hs);
        modelName = "Pet";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        CodegenModel pet = codegen.fromModel(modelName, sc);
        Assert.assertEquals(pet.discriminator, petDisc);

        // the Reptile discriminator contains both reptiles
        List<String> reptileModelNames = Arrays.asList("Lizard", "Snake");
        CodegenDiscriminator reptileDisc = new CodegenDiscriminator();
        reptileDisc.setPropertyName(propertyName);
        reptileDisc.setPropertyBaseName(propertyBaseName);
        hs.clear();
        for (String reptileModelName: reptileModelNames) {
            hs.add(new CodegenDiscriminator.MappedModel(reptileModelName, codegen.toModelName(reptileModelName)));
        }
        reptileDisc.setMappedModels(hs);
        modelName = "Reptile";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        CodegenModel reptile = codegen.fromModel(modelName, sc);
        Assert.assertEquals(reptile.discriminator, null);

        // the MyPets discriminator contains Cat and Lizard
        CodegenDiscriminator myPetDisc = new CodegenDiscriminator();
        myPetDisc.setPropertyName(propertyName);
        myPetDisc.setPropertyBaseName(propertyBaseName);
        hs.clear();
        modelName = "MyPets";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        CodegenModel myPets = codegen.fromModel(modelName, sc);
        Assert.assertEquals(myPets.discriminator, myPetDisc);

        // the MyPetsNoDisc discriminator is created because all oneOf classes have the same discriminator
        modelName = "MyPetsNoDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        CodegenModel myPetsNoDisc = codegen.fromModel(modelName, sc);
        Assert.assertEquals(myPetsNoDisc.discriminator, null);

        CodegenModel cm;

        // the mapping in b is in A
        modelName = "A";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs.clear();
        hs.add(new CodegenDiscriminator.MappedModel("b", codegen.toModelName("B")));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // the mapping in b is in B
        modelName = "B";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        Assert.assertEquals(cm.discriminator, null);

        // the mapping in b is in C
        modelName = "C";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        Assert.assertEquals(cm.discriminator, null);
    }

    @Test
    public void testComposedSchemaOneOfDiscriminatorsInvalid() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneOfDiscriminator.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setOpenAPI(openAPI);

        HashMap<String, String> hm = new HashMap<>();
        hm.put("ComposedDiscMissingNoProperties", "'ComposedDiscMissingNoProperties' defines discriminator 'fruitType', but the referenced schema 'DiscMissingNoProperties' is incorrect. fruitType is missing from the schema, define it as required and type string");
        hm.put("ComposedDiscMissingFromProperties", "'ComposedDiscMissingFromProperties' defines discriminator 'fruitType', but the referenced schema 'DiscMissingFromProperties' is incorrect. fruitType is missing from the schema, define it as required and type string");
        hm.put("ComposedDiscOptionalTypeCorrect", "'ComposedDiscOptionalTypeCorrect' defines discriminator 'fruitType', but the referenced schema 'DiscOptionalTypeCorrect' is incorrect. invalid optional definition of fruitType, include it in required");
        hm.put("ComposedDiscOptionalTypeIncorrect", "'ComposedDiscOptionalTypeIncorrect' defines discriminator 'fruitType', but the referenced schema 'DiscOptionalTypeIncorrect' is incorrect. invalid type for fruitType, set it to string. invalid optional definition of fruitType, include it in required");
        hm.put("ComposedDiscOptionalTypeInconsistent", "'ComposedDiscOptionalTypeInconsistent' defines discriminator 'fruitType', but the referenced schema 'DiscOptionalTypeIncorrect' is incorrect. invalid type for fruitType, set it to string. invalid optional definition of fruitType, include it in required");
        hm.put("ComposedDiscTypeIncorrect", "'ComposedDiscTypeIncorrect' defines discriminator 'fruitType', but the referenced schema 'DiscTypeIncorrect' is incorrect. invalid type for fruitType, set it to string");
        hm.put("ComposedDiscTypeInconsistent", "'ComposedDiscTypeInconsistent' defines discriminator 'fruitType', but the referenced schema 'DiscTypeIncorrect' is incorrect. invalid type for fruitType, set it to string");
        hm.put("ComposedDiscRequiredInconsistent", "'ComposedDiscRequiredInconsistent' defines discriminator 'fruitType', but the referenced schema 'DiscOptionalTypeCorrect' is incorrect. invalid optional definition of fruitType, include it in required");

        for(Map.Entry<String, String> entry : hm.entrySet()) {
            String modelName = entry.getKey();
            String errorMessageExpected = entry.getValue();

            Schema sc = openAPI.getComponents().getSchemas().get(modelName);

            try {
                codegen.fromModel(modelName, sc);
                Assert.assertTrue(false, "A RuntimeException should have been thrown when processing "+modelName+ " but it was not");
            } catch (RuntimeException re) {
                Assert.assertEquals(re.getMessage(), errorMessageExpected);
            }
        }
    }

    @Test
    public void testComposedSchemaAnyOfDiscriminatorsInvalid() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/anyOfDiscriminator.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setOpenAPI(openAPI);

        HashMap<String, String> hm = new HashMap<>();
        hm.put("ComposedDiscMissingNoProperties", "'ComposedDiscMissingNoProperties' defines discriminator 'fruitType', but the referenced schema 'DiscMissingNoProperties' is incorrect. fruitType is missing from the schema, define it as required and type string");
        hm.put("ComposedDiscMissingFromProperties", "'ComposedDiscMissingFromProperties' defines discriminator 'fruitType', but the referenced schema 'DiscMissingFromProperties' is incorrect. fruitType is missing from the schema, define it as required and type string");
        hm.put("ComposedDiscOptionalTypeCorrect", "'ComposedDiscOptionalTypeCorrect' defines discriminator 'fruitType', but the referenced schema 'DiscOptionalTypeCorrect' is incorrect. invalid optional definition of fruitType, include it in required");
        hm.put("ComposedDiscOptionalTypeIncorrect", "'ComposedDiscOptionalTypeIncorrect' defines discriminator 'fruitType', but the referenced schema 'DiscOptionalTypeIncorrect' is incorrect. invalid type for fruitType, set it to string. invalid optional definition of fruitType, include it in required");
        hm.put("ComposedDiscOptionalTypeInconsistent", "'ComposedDiscOptionalTypeInconsistent' defines discriminator 'fruitType', but the referenced schema 'DiscOptionalTypeIncorrect' is incorrect. invalid type for fruitType, set it to string. invalid optional definition of fruitType, include it in required");
        hm.put("ComposedDiscTypeIncorrect", "'ComposedDiscTypeIncorrect' defines discriminator 'fruitType', but the referenced schema 'DiscTypeIncorrect' is incorrect. invalid type for fruitType, set it to string");
        hm.put("ComposedDiscTypeInconsistent", "'ComposedDiscTypeInconsistent' defines discriminator 'fruitType', but the referenced schema 'DiscTypeIncorrect' is incorrect. invalid type for fruitType, set it to string");
        hm.put("ComposedDiscRequiredInconsistent", "'ComposedDiscRequiredInconsistent' defines discriminator 'fruitType', but the referenced schema 'DiscOptionalTypeCorrect' is incorrect. invalid optional definition of fruitType, include it in required");

        for(Map.Entry<String, String> entry : hm.entrySet()) {
            String modelName = entry.getKey();
            String errorMessageExpected = entry.getValue();

            Schema sc = openAPI.getComponents().getSchemas().get(modelName);

            try {
                codegen.fromModel(modelName, sc);
                Assert.assertTrue(false, "A RuntimeException should have been thrown when processing "+modelName+ " but it was not");
            } catch (RuntimeException re) {
                Assert.assertEquals(re.getMessage(), errorMessageExpected);
            }
        }
    }

    @Test
    public void testComposedSchemaAnyOfDiscriminatorMap() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/anyOfDiscriminator.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setOpenAPI(openAPI);

        String modelName;
        Schema sc;
        CodegenModel cm;
        java.util.LinkedHashSet hs;
        String mn;

        // inline anyOf models work because the inline schemas are turned into $refs
        modelName = "FruitInlineDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "FruitInlineDisc_anyOf";
        hs.add(new CodegenDiscriminator.MappedModel(mn, codegen.toModelName(mn)));
        mn = "FruitInlineDisc_anyOf_1";
        hs.add(new CodegenDiscriminator.MappedModel(mn, codegen.toModelName(mn)));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // inline anyOf with inline anyOf model doesn't work because we have null $refs and we throw an exception
        final String fmodelName = "FruitInlineInlineDisc";
        final Schema fsc = openAPI.getComponents().getSchemas().get(fmodelName);
        Assert.assertThrows(() -> codegen.fromModel(fmodelName, fsc));

        // ref anyOf models with discriminator in properties in those models
        modelName = "FruitReqDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "AppleReqDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        mn = "BananaReqDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // ref oneOf models with discriminator in allOf in those models
        modelName = "FruitAllOfDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "AppleAllOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        mn = "BananaAllOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // ref oneOf models with discriminator in anyOf in those models
        modelName = "FruitAnyOfDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "AppleAnyOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        mn = "BananaAnyOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // ref oneOf models with discriminator in anyOf in those models
        modelName = "FruitAnyOfDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "AppleAnyOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        mn = "BananaAnyOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // ref oneOf models with discriminator in the grandparent schemas of those anyof models
        modelName = "FruitGrandparentDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "AppleGrandparentDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        mn = "BananaGrandparentDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);
    }

    @Test
    public void testComposedSchemaOneOfDiscriminatorMap() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneOfDiscriminator.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setOpenAPI(openAPI);

        String modelName;
        Schema sc;
        CodegenModel cm;
        java.util.LinkedHashSet hs;
        String mn;

        // inline oneOf models work because the inline schemas are turned into $refs
        modelName = "FruitInlineDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "FruitInlineDisc_oneOf";
        hs.add(new CodegenDiscriminator.MappedModel(mn, codegen.toModelName(mn)));
        mn = "FruitInlineDisc_oneOf_1";
        hs.add(new CodegenDiscriminator.MappedModel(mn, codegen.toModelName(mn)));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // inline oneOf with inline oneOf model doesn't work because we have null $refs and we throw an exception
        final String fmodelName = "FruitInlineInlineDisc";
        final Schema fsc = openAPI.getComponents().getSchemas().get(fmodelName);
        Assert.assertThrows(() -> codegen.fromModel(fmodelName, fsc));

        // ref oneOf models with discriminator in properties in those models
        modelName = "FruitReqDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "AppleReqDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        mn = "BananaReqDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // ref oneOf models with discriminator in allOf in those models
        modelName = "FruitAllOfDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "AppleAllOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        mn = "BananaAllOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // ref oneOf models with discriminator in anyOf in those models
        modelName = "FruitAnyOfDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "AppleAnyOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        mn = "BananaAnyOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // ref oneOf models with discriminator in oneOf in those models
        modelName = "FruitOneOfDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "AppleOneOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        mn = "BananaOneOfDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);

        // ref oneOf models with discriminator in the grandparent schemas of those oneof models
        modelName = "FruitGrandparentDisc";
        sc = openAPI.getComponents().getSchemas().get(modelName);
        cm = codegen.fromModel(modelName, sc);
        hs = new java.util.LinkedHashSet();
        mn = "AppleGrandparentDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        mn = "BananaGrandparentDisc";
        hs.add(new CodegenDiscriminator.MappedModel(mn, mn));
        Assert.assertEquals(cm.discriminator.getMappedModels(), hs);
    }

    @Test
    public void testComposedSchemaMyPetsOneOfDiscriminatorMap() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf_composition_discriminator.yaml");

        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setOpenAPI(openAPI);

        String path = "/mypets";

        Operation operation = openAPI.getPaths().get(path).getGet();
        CodegenOperation codegenOperation = codegen.fromOperation(path, "GET", operation, null);
        verifyMyPetsDiscriminator(codegenOperation.discriminator);

        Schema pet = openAPI.getComponents().getSchemas().get("MyPets");
        CodegenModel petModel = codegen.fromModel("MyPets", pet);
        verifyMyPetsDiscriminator(petModel.discriminator);
    }

    @Test
    public void testComposedSchemaAllOfHierarchy(){
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf_composition_discriminator.yaml");

        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setLegacyDiscriminatorBehavior(false);
        codegen.setOpenAPI(openAPI);

        Schema pet = openAPI.getComponents().getSchemas().get("Lizard");
        CodegenModel petModel = codegen.fromModel("Lizard", pet);
        verifyLizardDiscriminator(petModel.discriminator);

        pet = openAPI.getComponents().getSchemas().get("Reptile");
        petModel = codegen.fromModel("Reptile", pet);
        verifyReptileDiscriminator(petModel.discriminator);
    }

    private void verifyLizardDiscriminator(CodegenDiscriminator discriminator) {
        CodegenDiscriminator test = new CodegenDiscriminator();
        String prop = "petType";
        test.setPropertyName(prop);
        test.setPropertyBaseName(prop);
        test.setMapping(null);
        test.setMappedModels(new HashSet<>());
        assertEquals(discriminator, test);
    }

    private void verifyReptileDiscriminator(CodegenDiscriminator discriminator) {
        CodegenDiscriminator test = new CodegenDiscriminator();
        String prop = "petType";
        test.setPropertyName(prop);
        test.setPropertyBaseName(prop);
        test.setMapping(null);
        test.setMappedModels(new HashSet<CodegenDiscriminator.MappedModel>(){{
            add(new CodegenDiscriminator.MappedModel("Snake", "Snake"));
            add(new CodegenDiscriminator.MappedModel("Lizard", "Lizard"));
        }});
        assertEquals(discriminator, test);
    }

    private void verifyMyPetsDiscriminator(CodegenDiscriminator discriminator) {
        CodegenDiscriminator test = new CodegenDiscriminator();
        String prop = "petType";
        test.setPropertyName(prop);
        test.setPropertyBaseName(prop);
        test.setMapping(null);
        test.setMappedModels(new HashSet<CodegenDiscriminator.MappedModel>(){{
            add(new CodegenDiscriminator.MappedModel("Cat", "Cat"));
            add(new CodegenDiscriminator.MappedModel("Lizard", "Lizard"));
        }});
        assertEquals(discriminator, test);
    }

    public CodegenModel getModel(List<Object> allModels, String modelName) {
        for (Object obj: allModels) {
            HashMap<String, Object> hm = (HashMap<String, Object>) obj;
            CodegenModel cm = (CodegenModel) hm.get("model");
            if (modelName.equals(cm.name)) {
                return cm;
            }
        }
        return null;
    }

    @Test
    public void verifyXDiscriminatorValue() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/x-discriminator-value.yaml");
        final DefaultCodegen config = new DefaultCodegen();
        config.setOpenAPI(openAPI);

        String modelName;
        CodegenDiscriminator discriminator;
        Schema sc;
        CodegenModel cm;

        Boolean dryRun = Boolean.TRUE;
        final DefaultGenerator generator = new DefaultGenerator(dryRun);
        generator.openAPI = openAPI;
        generator.config = config;
        generator.configureGeneratorProperties();

        // for us to check a model's children we need to run generator.generateModels
        // because children are assigned in config.updateAllModels which is invoked in generator.generateModels
        List<File> files = new ArrayList<>();
        List<String> filteredSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        List<Object> allModels = new ArrayList<>();
        generator.generateModels(files, allModels, filteredSchemas);

        // check that the model's children contain the x-discriminator-values
        modelName = "BaseObj";
        cm = getModel(allModels, modelName);
        Assert.assertNotNull(cm);
        Assert.assertNotNull(cm.children);
        List<String> excpectedDiscriminatorValues = new ArrayList<>(Arrays.asList("daily", "sub-obj"));
        ArrayList<String> xDiscriminatorValues = new ArrayList<>();
        for (CodegenModel child: cm.children) {
            xDiscriminatorValues.add((String) child.vendorExtensions.get("x-discriminator-value"));
        }
        assertEquals(xDiscriminatorValues, excpectedDiscriminatorValues);

        // check that the discriminator's MappedModels also contains the x-discriminator-values
        discriminator = new CodegenDiscriminator();
        String prop = "object_type";
        discriminator.setPropertyName(config.toVarName(prop));
        discriminator.setPropertyBaseName(prop);
        discriminator.setMapping(null);
        discriminator.setMappedModels(new HashSet<CodegenDiscriminator.MappedModel>(){{
            add(new CodegenDiscriminator.MappedModel("DailySubObj", "DailySubObj"));
            add(new CodegenDiscriminator.MappedModel("SubObj", "SubObj"));
            add(new CodegenDiscriminator.MappedModel("daily", "DailySubObj"));
            add(new CodegenDiscriminator.MappedModel("sub-obj", "SubObj"));
        }});
        assertEquals(cm.discriminator, discriminator);
    }


    @Test
    public void testAllOfSingleRefNoOwnProps() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/composed-allof.yaml");
        final DefaultCodegen codegen = new CodegenWithMultipleInheritance();

        Schema schema = openAPI.getComponents().getSchemas().get("NewMessageEventCoreNoOwnProps");
        codegen.setOpenAPI(openAPI);
        CodegenModel model = codegen.fromModel("NewMessageEventCoreNoOwnProps", schema);
        Assert.assertEquals(getNames(model.getVars()), Collections.emptyList());
        Assert.assertEquals(model.parent, "MessageEventCore");
        Assert.assertEquals(model.allParents, Collections.singletonList("MessageEventCore"));
    }

    class CodegenWithMultipleInheritance extends DefaultCodegen {
        public CodegenWithMultipleInheritance() {
            super();
            supportsInheritance = true;
            supportsMultipleInheritance = true;
        }
    }


    @Test
    public void testAllOfParent() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/allOf-required-parent.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Schema person = openAPI.getComponents().getSchemas().get("person");
        CodegenModel personModel = codegen.fromModel("person", person);
        Assert.assertEquals(getRequiredVars(personModel), Arrays.asList("firstName", "name", "email", "id"));

        Schema personForCreation = openAPI.getComponents().getSchemas().get("personForCreation");
        CodegenModel personForCreationModel = codegen.fromModel("personForCreation", personForCreation);
        Assert.assertEquals(getRequiredVars(personForCreationModel), Arrays.asList("firstName", "name", "email"));

        Schema personForUpdate = openAPI.getComponents().getSchemas().get("personForUpdate");
        CodegenModel personForUpdateModel = codegen.fromModel("personForUpdate", personForUpdate);
        Assert.assertEquals(getRequiredVars(personForUpdateModel), Collections.emptyList());
    }

    private List<String> getRequiredVars(CodegenModel model) {
        return getNames(model.getRequiredVars());
    }

    private List<String> getNames(List<CodegenProperty> props) {
        if(props == null) return null;
        return props.stream().map(v -> v.name).collect(Collectors.toList());
    }

    @Test
    public void testCallbacks() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/callbacks.yaml");
        final CodegenConfig codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        final String path = "/streams";
        Operation subscriptionOperation = openAPI.getPaths().get("/streams").getPost();
        CodegenOperation op = codegen.fromOperation(path, "post", subscriptionOperation, null);

        Assert.assertFalse(op.isCallbackRequest);
        Assert.assertNotNull(op.operationId);
        Assert.assertEquals(op.callbacks.size(), 2);

        CodegenCallback cbB = op.callbacks.get(1);
        Assert.assertEquals(cbB.name, "dummy");
        Assert.assertFalse(cbB.hasMore);
        Assert.assertEquals(cbB.urls.size(), 0);

        CodegenCallback cbA = op.callbacks.get(0);
        Assert.assertEquals(cbA.name, "onData");
        Assert.assertTrue(cbA.hasMore);

        Assert.assertEquals(cbA.urls.size(), 2);

        CodegenCallback.Url urlB = cbA.urls.get(1);
        Assert.assertEquals(urlB.expression, "{$request.query.callbackUrl}/test");
        Assert.assertFalse(urlB.hasMore);
        Assert.assertEquals(urlB.requests.size(), 0);

        CodegenCallback.Url urlA = cbA.urls.get(0);
        Assert.assertEquals(urlA.expression, "{$request.query.callbackUrl}/data");
        Assert.assertTrue(urlA.hasMore);
        Assert.assertEquals(urlA.requests.size(), 2);

        urlA.requests.forEach(req -> {
            Assert.assertTrue(req.isCallbackRequest);
            Assert.assertNotNull(req.bodyParam);
            Assert.assertEquals(req.responses.size(), 2);

            switch (req.httpMethod.toLowerCase(Locale.getDefault())) {
                case "post":
                    Assert.assertEquals(req.operationId, "onDataDataPost");
                    Assert.assertEquals(req.bodyParam.dataType, "NewNotificationData");
                    break;
                case "delete":
                    Assert.assertEquals(req.operationId, "onDataDataDelete");
                    Assert.assertEquals(req.bodyParam.dataType, "DeleteNotificationData");
                    break;
                default:
                    Assert.fail(String.format(Locale.getDefault(), "invalid callback request http method '%s'", req.httpMethod));
            }
        });
    }

    @Test
    public void testLeadingSlashIsAddedIfMissing() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        Operation operation1 = new Operation().operationId("op1").responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")));
        openAPI.path("/here", new PathItem().get(operation1));
        Operation operation2 = new Operation().operationId("op2").responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")));
        openAPI.path("some/path", new PathItem().get(operation2));
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenOperation co1 = codegen.fromOperation("/here", "get", operation2, null);
        Assert.assertEquals(co1.path, "/here");
        CodegenOperation co2 = codegen.fromOperation("some/path", "get", operation2, null);
        Assert.assertEquals(co2.path, "/some/path");
    }

    @Test
    public void testDefaultResponseShouldBeLast() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        Operation myOperation = new Operation().operationId("myOperation").responses(
            new ApiResponses()
            .addApiResponse(
                "default", new ApiResponse().description("Default"))
            .addApiResponse(
                "422", new ApiResponse().description("Error"))
            );
        openAPI.path("/here", new PathItem().get(myOperation));
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenOperation co = codegen.fromOperation("/here", "get", myOperation, null);
        Assert.assertEquals(co.responses.get(0).message, "Error");
        Assert.assertEquals(co.responses.get(1).message, "Default");
    }

    @Test
    public void testResponseWithNoSchemaInHeaders() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        ApiResponse response2XX = new ApiResponse()
                .description("OK")
                .addHeaderObject("x-custom-header", new Header()
                        .description("a custom header")
                        .style(Header.StyleEnum.SIMPLE));
        Operation operation1 = new Operation().operationId("op1").responses(new ApiResponses().addApiResponse("2XX", response2XX));
        openAPI.path("/here", new PathItem().get(operation1));
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenResponse cr = codegen.fromResponse("2XX", response2XX);
        Assert.assertNotNull(cr);
        Assert.assertTrue(cr.hasHeaders);
    }

    @Test
    public void testNullableProperty() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/examples.yaml");
        new InlineModelResolver().flatten(openAPI);
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenProperty property = codegen.fromProperty("address", (Schema) openAPI.getComponents().getSchemas().get("User").getProperties().get("address"));

        Assert.assertTrue(property.isNullable);
    }

    @Test
    public void testDeprecatedModel() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/component-deprecated.yml");
        new InlineModelResolver().flatten(openAPI);
        final DefaultCodegen codegen = new DefaultCodegen();

        CodegenModel codedenPetModel = codegen.fromModel("Pet", openAPI.getComponents().getSchemas().get("Pet"));
        Assert.assertTrue(codedenPetModel.isDeprecated);

        CodegenModel codegenFoodModel = codegen.fromModel("Food", openAPI.getComponents().getSchemas().get("Food"));
        Assert.assertTrue(codegenFoodModel.isDeprecated);
    }

    @Test
    public void testDeprecatedProperty() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/property-deplicated.yaml");
        new InlineModelResolver().flatten(openAPI);
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        final Map responseProperties = Collections.unmodifiableMap(openAPI.getComponents().getSchemas().get("Response").getProperties());
        final Map requestProperties = Collections.unmodifiableMap(openAPI.getComponents().getSchemas().get("Response").getProperties());

        Assert.assertTrue(codegen.fromProperty("firstName",(Schema) responseProperties.get("firstName")).deprecated);
        Assert.assertFalse(codegen.fromProperty("customerCode",(Schema) responseProperties.get("customerCode")).deprecated);
        Assert.assertTrue(codegen.fromProperty("firstName",(Schema) requestProperties.get("firstName")).deprecated);
        Assert.assertFalse(codegen.fromProperty("customerCode",(Schema) requestProperties.get("customerCode")).deprecated);
    }

    @Test
    public void testDeprecatedRef() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/model-deprecated.yaml");
        new InlineModelResolver().flatten(openAPI);
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        final Map requestProperties = Collections.unmodifiableMap(openAPI.getComponents().getSchemas().get("complex").getProperties());

        Assert.assertTrue(codegen.fromProperty("deprecated", (Schema)requestProperties.get("deprecated")).deprecated);
        Assert.assertFalse(codegen.fromProperty("current", (Schema)requestProperties.get("current")).deprecated);
    }

    @Test
    public void integerSchemaPropertyAndModelTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema schema = new IntegerSchema().format("int32");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        //Property:
        final CodegenProperty cp = codegen.fromProperty("someProperty", schema);
        Assert.assertEquals(cp.baseType, "integer");
        Assert.assertEquals(cp.baseName, "someProperty");
        Assert.assertFalse(cp.isString);
        Assert.assertTrue(cp.isInteger);
        Assert.assertFalse(cp.isLong);
        Assert.assertFalse(cp.isNumber);
        Assert.assertTrue(cp.isNumeric);
        Assert.assertFalse(cp.isFloat);
        Assert.assertFalse(cp.isDouble);

        //Model:
        final CodegenModel cm = codegen.fromModel("someModel", schema);
        Assert.assertEquals(cm.dataType, "integer");
        Assert.assertEquals(cm.name, "someModel");
        Assert.assertFalse(cm.isString);
        Assert.assertTrue(cm.isInteger);
        Assert.assertFalse(cm.isLong);
        Assert.assertFalse(cm.isNumber);
        Assert.assertTrue(cm.isNumeric);
        Assert.assertFalse(cm.isFloat);
        Assert.assertFalse(cm.isDouble);
    }

    @Test
    public void longSchemaPropertyAndModelTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema schema = new IntegerSchema().format("int64");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        //Property:
        final CodegenProperty cp = codegen.fromProperty("someProperty", schema);
        Assert.assertEquals(cp.baseType, "long");
        Assert.assertEquals(cp.baseName, "someProperty");
        Assert.assertFalse(cp.isString);
        Assert.assertFalse(cp.isInteger);
        Assert.assertTrue(cp.isLong);
        Assert.assertFalse(cp.isNumber);
        Assert.assertTrue(cp.isNumeric);
        Assert.assertFalse(cp.isFloat);
        Assert.assertFalse(cp.isDouble);

        //Model:
        final CodegenModel cm = codegen.fromModel("someModel", schema);
        Assert.assertEquals(cm.dataType, "long");
        Assert.assertEquals(cm.name, "someModel");
        Assert.assertFalse(cm.isString);
        Assert.assertFalse(cm.isInteger);
        Assert.assertTrue(cm.isLong);
        Assert.assertFalse(cm.isNumber);
        Assert.assertTrue(cm.isNumeric);
        Assert.assertFalse(cm.isFloat);
        Assert.assertFalse(cm.isDouble);
    }

    @Test
    public void numberSchemaPropertyAndModelTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema schema = new NumberSchema();
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        //Property:
        final CodegenProperty cp = codegen.fromProperty("someProperty", schema);
        Assert.assertEquals(cp.baseType, "number");
        Assert.assertEquals(cp.baseName, "someProperty");
        Assert.assertFalse(cp.isString);
        Assert.assertFalse(cp.isInteger);
        Assert.assertFalse(cp.isLong);
        Assert.assertTrue(cp.isNumber);
        Assert.assertTrue(cp.isNumeric);
        Assert.assertFalse(cp.isFloat);
        Assert.assertFalse(cp.isDouble);

        //Model:
        final CodegenModel cm = codegen.fromModel("someModel", schema);
        Assert.assertEquals(cm.dataType, "number");
        Assert.assertEquals(cm.name, "someModel");
        Assert.assertFalse(cm.isString);
        Assert.assertFalse(cm.isInteger);
        Assert.assertFalse(cm.isLong);
        Assert.assertTrue(cm.isNumber);
        Assert.assertTrue(cm.isNumeric);
        Assert.assertFalse(cm.isFloat);
        Assert.assertFalse(cm.isDouble);
    }

    @Test
    public void numberFloatSchemaPropertyAndModelTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema schema = new NumberSchema().format("float");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        //Property:
        final CodegenProperty cp = codegen.fromProperty("someProperty", schema);
        Assert.assertEquals(cp.baseType, "float");
        Assert.assertEquals(cp.baseName, "someProperty");
        Assert.assertFalse(cp.isString);
        Assert.assertFalse(cp.isInteger);
        Assert.assertFalse(cp.isLong);
        Assert.assertFalse(cp.isNumber);
        Assert.assertTrue(cp.isNumeric);
        Assert.assertTrue(cp.isFloat);
        Assert.assertFalse(cp.isDouble);

        //Model:
        final CodegenModel cm = codegen.fromModel("someModel", schema);
        Assert.assertEquals(cm.dataType, "float");
        Assert.assertEquals(cm.name, "someModel");
        Assert.assertFalse(cm.isString);
        Assert.assertFalse(cm.isInteger);
        Assert.assertFalse(cm.isLong);
        Assert.assertFalse(cm.isNumber);
        Assert.assertTrue(cm.isNumeric);
        Assert.assertTrue(cm.isFloat);
        Assert.assertFalse(cm.isDouble);
    }

    @Test
    public void numberDoubleSchemaPropertyAndModelTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema schema = new NumberSchema().format("double");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        //Property:
        final CodegenProperty cp = codegen.fromProperty("someProperty", schema);
        Assert.assertEquals(cp.baseType, "double");
        Assert.assertEquals(cp.baseName, "someProperty");
        Assert.assertFalse(cp.isString);
        Assert.assertFalse(cp.isInteger);
        Assert.assertFalse(cp.isLong);
        Assert.assertFalse(cp.isNumber);
        Assert.assertTrue(cp.isNumeric);
        Assert.assertFalse(cp.isFloat);
        Assert.assertTrue(cp.isDouble);

        //Model:
        final CodegenModel cm = codegen.fromModel("someModel", schema);
        Assert.assertEquals(cm.dataType, "double");
        Assert.assertEquals(cm.name, "someModel");
        Assert.assertFalse(cm.isString);
        Assert.assertFalse(cm.isInteger);
        Assert.assertFalse(cm.isLong);
        Assert.assertFalse(cm.isNumber);
        Assert.assertTrue(cm.isNumeric);
        Assert.assertFalse(cm.isFloat);
        Assert.assertTrue(cm.isDouble);
    }

    @Test
    public void testAlias() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/type_alias.yaml");
        new InlineModelResolver().flatten(openAPI);

        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenModel typeAliasModel = codegen.fromModel(
                "MyParameterTextField",
                openAPI.getComponents().getSchemas().get("MyParameterTextField")
        );
        Assert.assertTrue(typeAliasModel.isAlias);
        Assert.assertEquals("string", typeAliasModel.dataType);

        CodegenModel composedModel = codegen.fromModel(
                "ComposedModel",
                openAPI.getComponents().getSchemas().get("ComposedModel")
        );
        Assert.assertFalse(composedModel.isAlias);
    }

    private void verifyPersonDiscriminator(CodegenDiscriminator discriminator) {
        CodegenDiscriminator test = new CodegenDiscriminator();
        test.setPropertyName("DollarUnderscoretype");
        test.setPropertyBaseName("$_type");
        test.setMapping(new HashMap<>());
        test.getMapping().put("a", "#/components/schemas/Adult");
        test.getMapping().put("c", "Child");
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("a", "Adult"));
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("c", "Child"));
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("Adult", "Adult"));
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("Child", "Child"));
        Assert.assertEquals(discriminator, test);
    }

    private CodegenProperty codegenPropertyWithArrayOfIntegerValues() {
        CodegenProperty array = new CodegenProperty();
        final CodegenProperty items = new CodegenProperty();
        final HashMap<String, Object> allowableValues = new HashMap<>();
        allowableValues.put("values", Collections.singletonList(1));
        items.setAllowableValues(allowableValues);
        items.dataType = "Integer";
        array.items = items;
        array.mostInnerItems = items;
        array.dataType = "Array";
        return array;
    }

    private CodegenProperty codegenProperty(List<String> values) {
        CodegenProperty array = new CodegenProperty();
        final CodegenProperty items = new CodegenProperty();
        final HashMap<String, Object> allowableValues = new HashMap<>();
        allowableValues.put("values", values);
        items.setAllowableValues(allowableValues);
        items.dataType = "String";
        array.items = items;
        array.mostInnerItems = items;
        array.dataType = "Array";
        return array;
    }

    private CodegenProperty codegenPropertyWithXEnumVarName(List<String> values, List<String> aliases) {
        final CodegenProperty var = new CodegenProperty();
        final HashMap<String, Object> allowableValues = new HashMap<>();
        allowableValues.put("values", values);
        var.setAllowableValues(allowableValues);
        var.dataType = "String";
        Map<String, Object> extentions = Collections.singletonMap("x-enum-varnames", aliases);
        var.setVendorExtensions(extentions);
        return var;
    }

    private Map<String, Object> codegenModel(List<String> values) {
        final CodegenModel cm = new CodegenModel();
        cm.isEnum = true;
        final HashMap<String, Object> allowableValues = new HashMap<>();
        allowableValues.put("values", values);
        cm.setAllowableValues(allowableValues);
        cm.dataType = "String";
        Map<String, Object> objs = Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", cm)));
        return objs;
    }

    private Map<String, Object> codegenModelWithXEnumVarName() {
        final CodegenModel cm = new CodegenModel();
        cm.isEnum = true;
        final HashMap<String, Object> allowableValues = new HashMap<>();
        allowableValues.put("values", Arrays.asList("dog", "cat"));
        cm.setAllowableValues(allowableValues);
        cm.dataType = "String";
        final List<String> aliases = Arrays.asList("DOGVAR", "CATVAR");
        final List<String> descriptions = Arrays.asList("This is a dog", "This is a cat");
        Map<String, Object> extentions = new HashMap<>();
        extentions.put("x-enum-varnames", aliases);
        extentions.put("x-enum-descriptions", descriptions);
        cm.setVendorExtensions(extentions);
        cm.setVars(Collections.emptyList());
        Map<String, Object> objs = Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", cm)));
        return objs;
    }

    @Test
    public void objectQueryParamIdentifyAsObject() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/objectQueryParam.yaml");
        new InlineModelResolver().flatten(openAPI);
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Set<String> imports = new HashSet<>();
        CodegenParameter parameter = codegen.fromParameter(openAPI.getPaths().get("/pony").getGet().getParameters().get(0), imports);

        // TODO: This must be updated to work with flattened inline models
        Assert.assertEquals(parameter.dataType, "PageQuery1");
        Assert.assertEquals(imports.size(), 1);
        Assert.assertEquals(imports.iterator().next(), "PageQuery1");
    }

    @Test
    public void mapParamImportInnerObject() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/mapArgs.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        RequestBody requestBody = openAPI.getPaths().get("/api/instruments").getPost().getRequestBody();

        HashSet<String> imports = new HashSet<>();
        codegen.fromRequestBody(requestBody, imports, "");

        HashSet<String> expected = Sets.newHashSet("InstrumentDefinition", "map");

        Assert.assertEquals(imports, expected);
    }

    @Test
    public void modelDoNotContainInheritedVars() {
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/generic.yaml");
        codegen.setOpenAPI(openAPI);

        CodegenModel codegenModel = codegen.fromModel("Dog", openAPI.getComponents().getSchemas().get("Dog"));

        Assert.assertEquals(codegenModel.vars.size(), 1);
    }

    @Test
    public void importMapping() {
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.importMapping.put("TypeAlias", "foo.bar.TypeAlias");

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/type-alias.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOpenAPI(openAPI);

        CodegenModel codegenModel = codegen.fromModel("ParentType", openAPI.getComponents().getSchemas().get("ParentType"));

        Assert.assertEquals(codegenModel.vars.size(), 1);
        Assert.assertEquals(codegenModel.vars.get(0).getBaseType(), "TypeAlias");
    }

    @Test
    public void modelWithPrefixDoNotContainInheritedVars() {
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        codegen.setModelNamePrefix("prefix");

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/generic.yaml");
        codegen.setOpenAPI(openAPI);

        CodegenModel codegenModel = codegen.fromModel("Dog", openAPI.getComponents().getSchemas().get("Dog"));

        Assert.assertEquals(codegenModel.vars.size(), 1);
    }

    @Test
    public void modelWithSuffixDoNotContainInheritedVars() {
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        codegen.setModelNameSuffix("suffix");

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/generic.yaml");
        codegen.setOpenAPI(openAPI);

        CodegenModel codegenModel = codegen.fromModel("Dog", openAPI.getComponents().getSchemas().get("Dog"));

        Assert.assertEquals(codegenModel.vars.size(), 1);
    }

    @Test
    public void arrayInnerReferencedSchemaMarkedAsModel_20() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/arrayRefBody.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Set<String> imports = new HashSet<>();

        RequestBody body = openAPI.getPaths().get("/examples").getPost().getRequestBody();

        CodegenParameter codegenParameter = codegen.fromRequestBody(body, imports, "");

        Assert.assertTrue(codegenParameter.isContainer);
        Assert.assertTrue(codegenParameter.items.isModel);
        Assert.assertFalse(codegenParameter.items.isContainer);
    }

    @Test
    public void arrayInnerReferencedSchemaMarkedAsModel_30() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/arrayRefBody.yaml");
        new InlineModelResolver().flatten(openAPI);
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Set<String> imports = new HashSet<>();

        RequestBody body = openAPI.getPaths().get("/examples").getPost().getRequestBody();

        CodegenParameter codegenParameter = codegen.fromRequestBody(body, imports, "");

        Assert.assertTrue(codegenParameter.isContainer);
        Assert.assertTrue(codegenParameter.items.isModel);
        Assert.assertFalse(codegenParameter.items.isContainer);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void commonLambdasRegistrationTest() {

        DefaultCodegen codegen = new DefaultCodegen();
        Object lambdasObj = codegen.additionalProperties.get("lambda");

        assertNotNull(lambdasObj, "Expecting lambda in additionalProperties");

        Map<String, Lambda> lambdas = (Map<String, Lambda>) lambdasObj;

        assertTrue(lambdas.get("lowercase") instanceof LowercaseLambda, "Expecting LowercaseLambda class");
        assertTrue(lambdas.get("uppercase") instanceof UppercaseLambda, "Expecting UppercaseLambda class");
        assertTrue(lambdas.get("titlecase") instanceof TitlecaseLambda, "Expecting TitlecaseLambda class");
        assertTrue(lambdas.get("camelcase") instanceof CamelCaseLambda, "Expecting CamelCaseLambda class");
        assertTrue(lambdas.get("indented") instanceof IndentedLambda, "Expecting IndentedLambda class");
        assertTrue(lambdas.get("indented_8") instanceof IndentedLambda, "Expecting IndentedLambda class");
        assertTrue(lambdas.get("indented_12") instanceof IndentedLambda, "Expecting IndentedLambda class");
        assertTrue(lambdas.get("indented_16") instanceof IndentedLambda, "Expecting IndentedLambda class");
    }

    @Test
    public void convertApiNameWithEmptySuffix() {
        DefaultCodegen codegen = new DefaultCodegen();
        assertEquals(codegen.toApiName("Fake"), "FakeApi");
        assertEquals(codegen.toApiName(""), "DefaultApi");
    }

    @Test
    public void convertApiNameWithSuffix() {
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setApiNameSuffix("Test");
        assertEquals(codegen.toApiName("Fake"), "FakeTest");
        assertEquals(codegen.toApiName(""), "DefaultApi");
    }

    public static class FromParameter {
        private CodegenParameter codegenParameter(String path) {
            final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/fromParameter.yaml");
            new InlineModelResolver().flatten(openAPI);
            final DefaultCodegen codegen = new DefaultCodegen();
            codegen.setOpenAPI(openAPI);

            return codegen
                    .fromParameter(
                            openAPI
                                    .getPaths()
                                    .get(path)
                                    .getGet()
                                    .getParameters()
                                    .get(0),
                            new HashSet<>()
                    );
        }

        @Test
        public void setStyle() {
            CodegenParameter parameter = codegenParameter("/set_style");
            assertEquals("form", parameter.style);
        }

        @Test
        public void setShouldExplode() {
            CodegenParameter parameter = codegenParameter("/set_should_explode");
            assertTrue(parameter.isExplode);
        }

        @Test
        public void testConvertPropertyToBooleanAndWriteBack_Boolean_true() {
            final DefaultCodegen codegen = new DefaultCodegen();
            Map<String, Object> additionalProperties = codegen.additionalProperties();
            additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, true);
            boolean result = codegen.convertPropertyToBooleanAndWriteBack(CodegenConstants.SERIALIZABLE_MODEL);
            Assert.assertTrue(result);
        }

        @Test
        public void testConvertPropertyToBooleanAndWriteBack_Boolean_false() {
            final DefaultCodegen codegen = new DefaultCodegen();
            Map<String, Object> additionalProperties = codegen.additionalProperties();
            additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, false);
            boolean result = codegen.convertPropertyToBooleanAndWriteBack(CodegenConstants.SERIALIZABLE_MODEL);
            Assert.assertFalse(result);
        }

        @Test
        public void testConvertPropertyToBooleanAndWriteBack_String_true() {
            final DefaultCodegen codegen = new DefaultCodegen();
            Map<String, Object> additionalProperties = codegen.additionalProperties();
            additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, "true");
            boolean result = codegen.convertPropertyToBooleanAndWriteBack(CodegenConstants.SERIALIZABLE_MODEL);
            Assert.assertTrue(result);
        }

        @Test
        public void testConvertPropertyToBooleanAndWriteBack_String_false() {
            final DefaultCodegen codegen = new DefaultCodegen();
            Map<String, Object> additionalProperties = codegen.additionalProperties();
            additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, "false");
            boolean result = codegen.convertPropertyToBooleanAndWriteBack(CodegenConstants.SERIALIZABLE_MODEL);
            Assert.assertFalse(result);
        }

        @Test
        public void testConvertPropertyToBooleanAndWriteBack_String_blibb() {
            final DefaultCodegen codegen = new DefaultCodegen();
            Map<String, Object> additionalProperties = codegen.additionalProperties();
            additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, "blibb");
            boolean result = codegen.convertPropertyToBooleanAndWriteBack(CodegenConstants.SERIALIZABLE_MODEL);
            Assert.assertFalse(result);
        }
    }

    @Test
    public void testCircularReferencesDetection() {
        // given
        DefaultCodegen codegen = new DefaultCodegen();
        final CodegenProperty inboundOut = new CodegenProperty();
        inboundOut.baseName = "out";
        inboundOut.dataType = "RoundA";
        final CodegenProperty roundANext = new CodegenProperty();
        roundANext.baseName = "next";
        roundANext.dataType = "RoundB";
        final CodegenProperty roundBNext = new CodegenProperty();
        roundBNext.baseName = "next";
        roundBNext.dataType = "RoundC";
        final CodegenProperty roundCNext = new CodegenProperty();
        roundCNext.baseName = "next";
        roundCNext.dataType = "RoundA";
        final CodegenProperty roundCOut = new CodegenProperty();
        roundCOut.baseName = "out";
        roundCOut.dataType = "Outbound";
        final CodegenModel inboundModel = new CodegenModel();
        inboundModel.setDataType("Inbound");
        inboundModel.setAllVars(Collections.singletonList(inboundOut));
        final CodegenModel roundAModel = new CodegenModel();
        roundAModel.setDataType("RoundA");
        roundAModel.setAllVars(Collections.singletonList(roundANext));
        final CodegenModel roundBModel = new CodegenModel();
        roundBModel.setDataType("RoundB");
        roundBModel.setAllVars(Collections.singletonList(roundBNext));
        final CodegenModel roundCModel = new CodegenModel();
        roundCModel.setDataType("RoundC");
        roundCModel.setAllVars(Arrays.asList(roundCNext, roundCOut));
        final CodegenModel outboundModel = new CodegenModel();
        outboundModel.setDataType("Outbound");
        final Map<String, CodegenModel> models = new HashMap<>();
        models.put("Inbound", inboundModel);
        models.put("RoundA", roundAModel);
        models.put("RoundB", roundBModel);
        models.put("RoundC", roundCModel);
        models.put("Outbound", outboundModel);

        // when
        codegen.setCircularReferences(models);

        // then
        Assert.assertFalse(inboundOut.isCircularReference);
        Assert.assertTrue(roundANext.isCircularReference);
        Assert.assertTrue(roundBNext.isCircularReference);
        Assert.assertTrue(roundCNext.isCircularReference);
        Assert.assertFalse(roundCOut.isCircularReference);
    }

    @Test
    public void testUseOneOfInterfaces() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/composed-oneof.yaml");
        final DefaultCodegen cg = new DefaultCodegen();
        cg.setUseOneOfInterfaces(true);
        cg.preprocessOpenAPI(openAPI);

        // assert names of the response/request schema oneOf interfaces are as expected
        Assert.assertEquals(
                openAPI.getPaths()
                        .get("/state")
                        .getPost()
                        .getRequestBody()
                        .getContent()
                        .get("application/json")
                        .getSchema()
                        .getExtensions()
                        .get("x-one-of-name"),
                "CreateState"
        );
        Assert.assertEquals(
                openAPI.getPaths()
                        .get("/state")
                        .getGet()
                        .getResponses()
                        .get("200")
                        .getContent()
                        .get("application/json")
                        .getSchema()
                        .getExtensions()
                        .get("x-one-of-name"),
                "GetState200"
        );
        // for the array schema, assert that a oneOf interface was added to schema map
        Schema items = ((ArraySchema) openAPI.getComponents().getSchemas().get("CustomOneOfArraySchema")).getItems();
        Assert.assertEquals(items.getExtensions().get("x-one-of-name"), "CustomOneOfArraySchemaOneOf");
    }
}
