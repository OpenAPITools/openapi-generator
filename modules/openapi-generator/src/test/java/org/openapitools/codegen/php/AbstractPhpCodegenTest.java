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

package org.openapitools.codegen.php;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.languages.AbstractPhpCodegen;
import org.openapitools.codegen.TestUtils;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;

public class AbstractPhpCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final AbstractPhpCodegen codegen = new P_AbstractPhpCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "php\\Model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "php\\Model");
        Assert.assertEquals(codegen.apiPackage(), "php\\Api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "php\\Api");
        Assert.assertEquals(codegen.getInvokerPackage(), "php");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "php");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final AbstractPhpCodegen codegen = new P_AbstractPhpCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.setModelPackage("My\\Client\\Model");
        codegen.setApiPackage("My\\Client\\Api");
        codegen.setInvokerPackage("My\\Client\\Invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "My\\Client\\Model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "My\\Client\\Model");
        Assert.assertEquals(codegen.apiPackage(), "My\\Client\\Api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE),"My\\Client\\Api");
        Assert.assertEquals(codegen.getInvokerPackage(), "My\\Client\\Invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "My\\Client\\Invoker");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final AbstractPhpCodegen codegen = new P_AbstractPhpCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "PHPmodel");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "PHPapi");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "PHPinvoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "PHPinvoker\\PHPmodel");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "PHPinvoker\\PHPmodel");
        Assert.assertEquals(codegen.apiPackage(), "PHPinvoker\\PHPapi");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "PHPinvoker\\PHPapi");
        Assert.assertEquals(codegen.getInvokerPackage(), "PHPinvoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "PHPinvoker");
    }

    @Test
    public void testEscapeMediaType() throws Exception {
        HashMap<String, String> all = new HashMap<>();
        all.put("mediaType", "*/*");
        HashMap<String, String> applicationJson = new HashMap<>();
        applicationJson.put("mediaType", "application/json");

        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.hasProduces = true;
        codegenOperation.produces = Arrays.asList(all, applicationJson);

        final AbstractPhpCodegen codegen = new P_AbstractPhpCodegen();
        codegen.escapeMediaType(Arrays.asList(codegenOperation));

        Assert.assertEquals(codegenOperation.produces.get(0).get("mediaType"), "*_/_*");
        Assert.assertEquals(codegenOperation.produces.get(1).get("mediaType"), "application/json");
    }

    @Test(dataProvider = "composerNames", description = "Issue #9998")
    public void testGetComposerPackageName(String gitUserId, String gitRepoId, String result) {
        final AbstractPhpCodegen codegen = new P_AbstractPhpCodegen();
        codegen.processOpts();

        codegen.setGitUserId(gitUserId);
        codegen.setGitRepoId(gitRepoId);
        Assert.assertEquals(codegen.getComposerPackageName(), result);
    }

    @DataProvider(name = "composerNames")
    public static Object[][] composerNames() {
        return new Object[][] {
            {"", "", ""},
            {"null", "null", ""},
            {"GIT_REPO_ID", "GIT_USER_ID", ""},
            {"git_repo_id", "git_user_id", "git_repo_id/git_user_id"},
            {"foo", "bar", "foo/bar"},
        };
    }

    @Test(description = "Issue #8945")
    public void testArrayOfArrays() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_8945.yaml");
        final AbstractPhpCodegen codegen = new P_AbstractPhpCodegen();

        Schema test1 = openAPI.getComponents().getSchemas().get("MyResponse");
        CodegenModel cm1 = codegen.fromModel("MyResponse", test1);

        // Make sure we got the container object.
        Assert.assertEquals(cm1.getDataType(), "object");
        Assert.assertEquals(codegen.getTypeDeclaration("MyResponse"), "\\php\\Model\\MyResponse");

        // Assert the array type is properly detected.
        CodegenProperty cp1 = cm1.vars.get(0);
        cp1 = codegen.fromProperty("ArrayProp", test1);
        Assert.assertTrue(cp1.isPrimitiveType);
    }

    @Test(description = "Issue #10244")
    public void testEnumPropertyWithDefaultValue() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/php/issue_10244.yaml");
        final AbstractPhpCodegen codegen = new P_AbstractPhpCodegen();

        Schema test1 = openAPI.getComponents().getSchemas().get("ModelWithEnumPropertyHavingDefault");
        CodegenModel cm1 = codegen.fromModel("ModelWithEnumPropertyHavingDefault", test1);

        // Make sure we got the container object.
        Assert.assertEquals(cm1.getDataType(), "object");
        Assert.assertEquals(codegen.getTypeDeclaration("MyResponse"), "\\php\\Model\\MyResponse");

        // We need to postProcess the model for enums to be processed
        codegen.postProcessModels(Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", cm1))));

        // Assert the enum default value is properly generated
        CodegenProperty cp1 = cm1.vars.get(0);
        Assert.assertEquals(cp1.getDefaultValue(), "'VALUE'");
    }

    private static class P_AbstractPhpCodegen extends AbstractPhpCodegen {
        @Override
        public CodegenType getTag() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String getHelp() {
            return null;
        }
    }
}
