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
import org.mockito.Answers;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.AbstractPhpCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.HashMap;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;

public class AbstractPhpCodegenTest {

    private AbstractPhpCodegen codegen;

    /**
     * In TEST-NG, test class (and its fields) is only constructed once (vs. for every test in Jupiter),
     * using @BeforeMethod to have a fresh codegen mock for each test
     */
    @BeforeMethod
    void mockAbstractCodegen() {
        codegen = mock(
                AbstractPhpCodegen.class, withSettings().defaultAnswer(Answers.CALLS_REAL_METHODS).useConstructor()
        );
    }

    @Test
    public void testInitialConfigValues() throws Exception {
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
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "My\\Client\\Api");
        Assert.assertEquals(codegen.getInvokerPackage(), "My\\Client\\Invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "My\\Client\\Invoker");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
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

        codegen.escapeMediaType(Arrays.asList(codegenOperation));

        Assert.assertEquals(codegenOperation.produces.get(0).get("mediaType"), "*_/_*");
        Assert.assertEquals(codegenOperation.produces.get(1).get("mediaType"), "application/json");
    }

    @Test(dataProvider = "composerNames", description = "Issue #9998")
    public void testGetComposerPackageName(String gitUserId, String gitRepoId, String result) {
        codegen.processOpts();

        codegen.setGitUserId(gitUserId);
        codegen.setGitRepoId(gitRepoId);
        Assert.assertEquals(codegen.getComposerPackageName(), result);
    }

    @DataProvider(name = "composerNames")
    public static Object[][] composerNames() {
        return new Object[][]{
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
        codegen.setOpenAPI(openAPI);

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
        codegen.setOpenAPI(openAPI);

        Schema test1 = openAPI.getComponents().getSchemas().get("ModelWithEnumPropertyHavingDefault");
        CodegenModel cm1 = codegen.fromModel("ModelWithEnumPropertyHavingDefault", test1);

        // Make sure we got the container object.
        Assert.assertEquals(cm1.getDataType(), "object");
        Assert.assertEquals(codegen.getTypeDeclaration("MyResponse"), "\\php\\Model\\MyResponse");

        // We need to postProcess the model for enums to be processed
        codegen.postProcessModels(TestUtils.createCodegenModelWrapper(cm1));

        // Assert the enum default value is properly generated
        CodegenProperty cp1 = cm1.vars.get(0);
        Assert.assertEquals(cp1.getDefaultValue(), "'VALUE'");
    }

    @Test(description = "Enum value with quotes (#17582)")
    public void testEnumPropertyWithQuotes() {
        Assert.assertEquals(codegen.toEnumValue("enum-value", "string"), "'enum-value'");
        Assert.assertEquals(codegen.toEnumValue("won't fix", "string"), "'won\\'t fix'");
        Assert.assertEquals(codegen.toEnumValue("\"", "string"), "'\"'");
        Assert.assertEquals(codegen.toEnumValue("1.0", "float"), "1.0");
        Assert.assertEquals(codegen.toEnumValue("1", "int"), "1");
    }
}
