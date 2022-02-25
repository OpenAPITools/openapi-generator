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

package org.openapitools.codegen.javascript;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.JavascriptClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class JavascriptClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), null);
        Assert.assertEquals(codegen.apiPackage(), "api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), null);
        Assert.assertEquals(codegen.getInvokerPackage(), null);
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), null);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test(description = "test defaultValueWithParam for model's properties")
    public void bodyParameterTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore.yaml");
        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        final Schema pet = openAPI.getComponents().getSchemas().get("Pet");
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("Pet", pet);

        Assert.assertEquals(cm.name, "Pet");
        Assert.assertEquals(cm.classname, "Pet");
        Assert.assertEquals(cm.description, "A pet for sale in the pet store");
        Assert.assertEquals(cm.vars.size(), 6);

        // category (property)
        final CodegenProperty property1 = cm.vars.get(1);
        Assert.assertEquals(property1.baseName, "category");
        Assert.assertEquals(property1.dataType, "Category");
        Assert.assertEquals(property1.name, "category");
        Assert.assertEquals(property1.baseType, "Category");
        Assert.assertEquals(property1.defaultValueWithParam, " = Category.constructFromObject(data['category']);");
        Assert.assertFalse(property1.required);
        Assert.assertFalse(property1.isContainer);

        // name (property)
        final CodegenProperty property2 = cm.vars.get(2);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "String");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property2.baseType, "String");
        Assert.assertEquals(property2.defaultValueWithParam, " = ApiClient.convertToType(data['name'], 'String');");
        Assert.assertTrue(property2.required); // test required
        Assert.assertFalse(property2.isContainer);
    }

    @Test(description = "test isDefault in the response")
    public void testResponseIsDefault() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setOpenAPI(openAPI);

        Operation textOperation = openAPI.getPaths().get("/user").getPost();
        CodegenOperation coText = codegen.fromOperation("/user", "post", textOperation, null);

        for (CodegenResponse cr : coText.responses) {
            Assert.assertTrue(cr.isDefault);
        }

        Assert.assertEquals(coText.responses.size(), 1);

    }

    @Test(description = "test multiple file upload collection is correct")
    public void testMultipleFileUpload() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/form-multipart-binary-array.yaml");
        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.setOpenAPI(openAPI);

        final String requestPath = "/multipart-array";
        Operation textOperation = openAPI.getPaths().get(requestPath).getPost();
        CodegenOperation operation = codegen.fromOperation(requestPath, "post", textOperation, null);
        CodegenParameter codegenParameter = operation.allParams.get(0);

        Assert.assertEquals(codegenParameter.collectionFormat, "passthrough");
    }

}
