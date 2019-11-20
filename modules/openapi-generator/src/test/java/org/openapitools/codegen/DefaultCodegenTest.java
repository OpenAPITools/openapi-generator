/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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

import org.openapitools.codegen.templating.mustache.CamelCaseLambda;
import org.openapitools.codegen.templating.mustache.IndentedLambda;
import org.openapitools.codegen.templating.mustache.LowercaseLambda;
import org.openapitools.codegen.templating.mustache.TitlecaseLambda;
import org.openapitools.codegen.templating.mustache.UppercaseLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/produces.yaml");
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Schema requestBodySchema = ModelUtils.getSchemaFromRequestBody(openAPI.getPaths().get("/fake").getGet().getRequestBody());
        CodegenParameter codegenParameter = codegen.fromFormProperty("enum_form_string", (Schema) requestBodySchema.getProperties().get("enum_form_string"), new HashSet<String>());

        Assert.assertEquals(codegenParameter.defaultValue, "-efg");
    }

    @Test
    public void testEnsureNoDuplicateProduces() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/two-responses.yaml");
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/composed-oneof.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/state").getPost();
        Schema schema = ModelUtils.getSchemaFromRequestBody(operation.getRequestBody());
        String type = codegen.getSchemaType(schema);

        Assert.assertNotNull(type);
        Assert.assertEquals(type, "oneOf<ObjA,ObjB>");
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/examples.yaml");
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/examples.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();

        Operation operation = openAPI.getPaths().get("/example2/singular").getGet();
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegen.setParameterExampleValue(codegenParameter, operation.getParameters().get(0));

        Assert.assertEquals(codegenParameter.example, "example2 value");
    }

    @Test
    public void testExample3() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/examples.yaml");
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/examples.yaml");
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        DefaultCodegen codegen = new DefaultCodegen();

        Schema animal = openAPI.getComponents().getSchemas().get("Animal");
        codegen.setOpenAPI(openAPI);
        CodegenModel animalModel = codegen.fromModel("Animal", animal);
        CodegenDiscriminator discriminator = animalModel.getDiscriminator();
        CodegenDiscriminator test = new CodegenDiscriminator();
        test.setPropertyName("className");
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("Dog", "Dog"));
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("Cat", "Cat"));
        Assert.assertEquals(discriminator, test);
    }

    @Test
    public void testDiscriminatorWithCustomMapping() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/allOf.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/allOf.yaml");
        DefaultCodegen codegen = new DefaultCodegen();

        Schema child = openAPI.getComponents().getSchemas().get("Child");
        codegen.setOpenAPI(openAPI);
        CodegenModel childModel = codegen.fromModel("Child", child);
        Assert.assertEquals(childModel.parentSchema, "Person");
    }

    @Test
    public void testAllOfRequired() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/allOf-required.yaml");
        DefaultCodegen codegen = new DefaultCodegen();

        Schema child = openAPI.getComponents().getSchemas().get("clubForCreation");
        codegen.setOpenAPI(openAPI);
        CodegenModel childModel = codegen.fromModel("clubForCreation", child);
        showVars(childModel);
    }

    @Test
    public void testAllOfParent() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/allOf-required-parent.yaml");
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Schema person = openAPI.getComponents().getSchemas().get("person");
        CodegenModel personModel = codegen.fromModel("person", person);
        showVars(personModel);

        Schema personForCreation = openAPI.getComponents().getSchemas().get("personForCreation");
        CodegenModel personForCreationModel = codegen.fromModel("personForCreation", personForCreation);
        showVars(personForCreationModel);

        Schema personForUpdate = openAPI.getComponents().getSchemas().get("personForUpdate");
        CodegenModel personForUpdateModel = codegen.fromModel("personForUpdate", personForUpdate);
        showVars(personForUpdateModel);
    }

    private void showVars(CodegenModel model) {
        if(model.getRequiredVars() != null) {

            System.out.println(model.getRequiredVars().stream().map(v -> v.name).collect(Collectors.toList()));
        }
    }


    @Test
    public void testCallbacks() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/callbacks.yaml");
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/examples.yaml");
        new InlineModelResolver().flatten(openAPI);
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenProperty property = codegen.fromProperty("address", (Schema) openAPI.getComponents().getSchemas().get("User").getProperties().get("address"));

        Assert.assertTrue(property.isNullable);
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

    private void verifyPersonDiscriminator(CodegenDiscriminator discriminator) {
        CodegenDiscriminator test = new CodegenDiscriminator();
        test.setPropertyName("DollarUnderscoretype");
        test.setMapping(new HashMap<>());
        test.getMapping().put("a", "#/components/schemas/Adult");
        test.getMapping().put("c", "Child");
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("a", "Adult"));
        test.getMappedModels().add(new CodegenDiscriminator.MappedModel("c", "Child"));
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/objectQueryParam.yaml");
        new InlineModelResolver().flatten(openAPI);
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Set<String> imports = new HashSet<>();
        CodegenParameter parameter = codegen.fromParameter(openAPI.getPaths().get("/pony").getGet().getParameters().get(0), imports);

        Assert.assertEquals(parameter.dataType, "PageQuery");
        Assert.assertEquals(imports.size(), 1);
        Assert.assertEquals(imports.iterator().next(), "PageQuery");
    }

    @Test
    public void mapParamImportInnerObject() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/2_0/mapArgs.yaml");
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

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/generic.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOpenAPI(openAPI);

        CodegenModel codegenModel = codegen.fromModel("Dog", openAPI.getComponents().getSchemas().get("Dog"));

        Assert.assertEquals(codegenModel.vars.size(), 1);
    }

    @Test
    public void modelWithPrefixDoNotContainInheritedVars() {
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        codegen.setModelNamePrefix("prefix");

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/generic.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOpenAPI(openAPI);

        CodegenModel codegenModel = codegen.fromModel("Dog", openAPI.getComponents().getSchemas().get("Dog"));

        Assert.assertEquals(codegenModel.vars.size(), 1);
    }

    @Test
    public void modelWithSuffixDoNotContainInheritedVars() {
        DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        codegen.setModelNameSuffix("suffix");

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/generic.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOpenAPI(openAPI);

        CodegenModel codegenModel = codegen.fromModel("Dog", openAPI.getComponents().getSchemas().get("Dog"));

        Assert.assertEquals(codegenModel.vars.size(), 1);
    }

    @Test
    public void arrayInnerReferencedSchemaMarkedAsModel_20() {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/2_0/arrayRefBody.yaml");
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
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/arrayRefBody.yaml");
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
            final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/fromParameter.yaml");
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
}
