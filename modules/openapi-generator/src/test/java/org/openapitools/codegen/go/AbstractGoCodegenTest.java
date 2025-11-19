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

package org.openapitools.codegen.go;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.mockito.Answers;
import org.openapitools.codegen.*;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.AbstractGoCodegen;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.model.WebhooksMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;

public class AbstractGoCodegenTest {

    private AbstractGoCodegen codegen;

    /**
     * In TEST-NG, test class (and its fields) is only constructed once (vs. for every test in Jupiter),
     * using @BeforeMethod to have a fresh codegen mock for each test
     */
    @BeforeMethod
    void mockAbstractCodegen() {
        codegen = mock(
                AbstractGoCodegen.class, withSettings().defaultAnswer(Answers.CALLS_REAL_METHODS).useConstructor()
        );
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        codegen.setHideGenerationTimestamp(true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void getTypeDeclarationTest() {

        // Create an alias to an array schema
        Schema<?> nestedArraySchema = new ArraySchema().items(new IntegerSchema().format("int32"));
        codegen.setOpenAPI(new OpenAPI().components(new Components().addSchemas("NestedArray", nestedArraySchema)));

        // Create an array schema with item type set to the array alias
        Schema<?> schema = new ArraySchema().items(new Schema().$ref("#/components/schemas/NestedArray"));

        ModelUtils.setGenerateAliasAsModel(false);
        String defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "[][]int32");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "[]NestedArray");

        // Create a map schema with additionalProperties type set to array alias
        schema = new MapSchema().additionalProperties(new Schema().$ref("#/components/schemas/NestedArray"));

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "map[string][]int32");

        ModelUtils.setGenerateAliasAsModel(true);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "map[string]NestedArray");

        // Create object schema with additionalProperties set to true
        schema = new ObjectSchema().additionalProperties(Boolean.TRUE);

        ModelUtils.setGenerateAliasAsModel(false);
        defaultValue = codegen.getTypeDeclaration(schema);
        Assert.assertEquals(defaultValue, "map[string]interface{}");
    }

    @Test(description = "test that os import is added for array of binary parameters in operations")
    public void testOsImportForArrayOfBinaryParametersInOperations() {
        // Create OpenAPI spec with array of binary files
        OpenAPI openAPI = new OpenAPI();
        openAPI.setComponents(new Components());
        
        // Create operation with array of binary parameter in request body
        Operation operation = new Operation();
        RequestBody requestBody = new RequestBody();
        Content content = new Content();
        MediaType mediaType = new MediaType();
        
        Schema<?> arraySchema = new ArraySchema().items(
            new BinarySchema()
        );
        
        ObjectSchema objectSchema = new ObjectSchema();
        objectSchema.addProperty("files", arraySchema);
        mediaType.setSchema(objectSchema);
        content.addMediaType("multipart/form-data", mediaType);
        requestBody.setContent(content);
        operation.setRequestBody(requestBody);
        
        codegen.setOpenAPI(openAPI);
        
        // Convert to CodegenOperation
        CodegenOperation codegenOperation = codegen.fromOperation("/upload", "post", operation, null);
        
        // Create OperationsMap structure
        OperationMap operationMap = new OperationMap();
        operationMap.setOperation(codegenOperation);
        OperationsMap operationsMap = new OperationsMap();
        operationsMap.setOperation(operationMap);
        operationsMap.setImports(new ArrayList<>());
        
        // Post-process the operations
        OperationsMap result = codegen.postProcessOperationsWithModels(operationsMap, Collections.emptyList());
        
        // Assert that "os" import was added
        List<Map<String, String>> imports = result.getImports();
        boolean hasOsImport = imports.stream()
            .anyMatch(imp -> "os".equals(imp.get("import")));
        
        Assert.assertTrue(hasOsImport, "Expected 'os' import to be added for array of binary files");
    }
    
    @Test(description = "test that time import is added for array of date-time parameters in operations")
    public void testTimeImportForArrayOfDateTimeParametersInOperations() {
        // Create OpenAPI spec with array of date-time parameter
        OpenAPI openAPI = new OpenAPI();
        openAPI.setComponents(new Components());
        
        Operation operation = new Operation();
        Parameter parameter = new Parameter();
        parameter.setName("timestamps");
        parameter.setIn("query");
        
        ArraySchema arraySchema = new ArraySchema();
        DateTimeSchema dateTimeSchema = new DateTimeSchema();
        arraySchema.setItems(dateTimeSchema);
        parameter.setSchema(arraySchema);
        
        operation.addParametersItem(parameter);
        
        codegen.setOpenAPI(openAPI);
        
        // Convert to CodegenOperation
        CodegenOperation codegenOperation = codegen.fromOperation("/events", "get", operation, null);
        
        // Create OperationsMap structure
        OperationMap operationMap = new OperationMap();
        operationMap.setOperation(codegenOperation);
        OperationsMap operationsMap = new OperationsMap();
        operationsMap.setOperation(operationMap);
        operationsMap.setImports(new ArrayList<>());
        
        // Post-process the operations
        OperationsMap result = codegen.postProcessOperationsWithModels(operationsMap, Collections.emptyList());
        
        // Assert that "time" import was added
        List<Map<String, String>> imports = result.getImports();
        boolean hasTimeImport = imports.stream()
            .anyMatch(imp -> "time".equals(imp.get("import")));
        
        Assert.assertTrue(hasTimeImport, "Expected 'time' import to be added for array of date-time parameters");
    }
}
