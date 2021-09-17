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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.GoDeprecatedClientCodegen;
import org.openapitools.codegen.languages.GoClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class GoClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final GoDeprecatedClientCodegen codegen = new GoDeprecatedClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final GoDeprecatedClientCodegen codegen = new GoDeprecatedClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final GoDeprecatedClientCodegen codegen = new GoDeprecatedClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test(description = "test example value for body parameter")
    public void bodyParameterTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final GoDeprecatedClientCodegen codegen = new GoDeprecatedClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/fake";
        final Operation p = openAPI.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);
        Assert.assertEquals(op.formParams.size(), 2);
        CodegenParameter bp = op.formParams.get(0);
        Assert.assertFalse(bp.isPrimitiveType);
    }

    @Test(description = "test to ensure the parameter names are unique")
    public void ensureParameterNameUniqueTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/conflictingParameter.yaml");
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/pet/{id}";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);
        Assert.assertEquals(op.allParams.size(), 9);
        CodegenParameter cp = op.allParams.get(0);
        Assert.assertEquals(cp.paramName, "id");
        CodegenParameter cp2 = op.allParams.get(1);
        Assert.assertEquals(cp2.paramName, "id2");
        CodegenParameter cp3 = op.allParams.get(2);
        Assert.assertEquals(cp3.paramName, "id3");
        CodegenParameter cp4 = op.allParams.get(3);
        Assert.assertEquals(cp4.paramName, "id4");
        CodegenParameter cp5 = op.allParams.get(4);
        Assert.assertEquals(cp5.paramName, "id5");
    }

    @Test
    public void testFilenames() throws Exception {
        final GoDeprecatedClientCodegen codegen = new GoDeprecatedClientCodegen();

        // Model names are generated from schema / definition names
        Assert.assertEquals(codegen.toModelFilename("Animal"), "model_animal");
        Assert.assertEquals(codegen.toModelFilename("AnimalTest"), "model_animal_test_");
        Assert.assertEquals(codegen.toModelFilename("AnimalFarm"), "model_animal_farm");
        Assert.assertEquals(codegen.toModelFilename("AnimalFarmTest"), "model_animal_farm_test_");

        // API names are generated from tag names
        Assert.assertEquals(codegen.toApiFilename("Animal"), "api_animal");
        Assert.assertEquals(codegen.toApiFilename("Animal Test"), "api_animal_test_");
        Assert.assertEquals(codegen.toApiFilename("Animal Farm"), "api_animal_farm");
        Assert.assertEquals(codegen.toApiFilename("Animal Farm Test"), "api_animal_farm_test_");
    }

}
