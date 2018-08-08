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

package org.openapitools.codegen.java;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class AbstractJavaCodegenTest {

    private final AbstractJavaCodegen fakeJavaCodegen = new P_AbstractJavaCodegen();

    @Test
    public void toEnumVarNameShouldNotShortenUnderScore() throws Exception {
        Assert.assertEquals("UNDERSCORE", fakeJavaCodegen.toEnumVarName("_", "String"));
        Assert.assertEquals("__", fakeJavaCodegen.toEnumVarName("__", "String"));
        Assert.assertEquals("__", fakeJavaCodegen.toEnumVarName("_,.", "String"));
    }

    @Test
    public void toVarNameShouldAvoidOverloadingGetClassMethod() throws Exception {
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("class"));
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("_class"));
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("__class"));
    }

    @Test
    public void toModelNameShouldUseProvidedMapping() throws Exception {
        fakeJavaCodegen.importMapping().put("json_myclass", "com.test.MyClass");
        Assert.assertEquals("com.test.MyClass", fakeJavaCodegen.toModelName("json_myclass"));
    }

    @Test
    public void toModelNameUsesPascalCase() throws Exception {
        Assert.assertEquals("JsonAnotherclass", fakeJavaCodegen.toModelName("json_anotherclass"));
    }
 
    @Test
    public void testPreprocessOpenAPI() throws Exception {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();

        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(openAPI.getPaths().get("/pet").getPost().getExtensions().get("x-accepts"), "application/json");
    }

    @Test
    public void convertVarName() throws Exception {
       Assert.assertEquals(fakeJavaCodegen.toVarName("name"), "name");
       Assert.assertEquals(fakeJavaCodegen.toVarName("$name"), "$name");
       Assert.assertEquals(fakeJavaCodegen.toVarName("nam$$e"), "nam$$e");
       Assert.assertEquals(fakeJavaCodegen.toVarName("user-name"), "userName");
       Assert.assertEquals(fakeJavaCodegen.toVarName("user_name"), "userName");
       Assert.assertEquals(fakeJavaCodegen.toVarName("user|name"), "userName");
   }

   @Test
   public void convertModelName() throws Exception {
       Assert.assertEquals(fakeJavaCodegen.toModelName("name"), "Name");
       Assert.assertEquals(fakeJavaCodegen.toModelName("$name"), "Name");
       Assert.assertEquals(fakeJavaCodegen.toModelName("nam#e"), "Name");
       Assert.assertEquals(fakeJavaCodegen.toModelName("$another-fake?"), "AnotherFake");
   }

    @Test
    public void testInitialConfigValues() throws Exception {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "invalidPackageName");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "invalidPackageName");
        Assert.assertEquals(codegen.apiPackage(), "invalidPackageName");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "invalidPackageName");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX), "get");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.USE_NULL_FOR_UNKNOWN_ENUM_VALUE), Boolean.FALSE);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xyz.yyyyy.zzzzzzz.model");
        codegen.setApiPackage("xyz.yyyyy.zzzzzzz.api");
        codegen.setInvokerPackage("xyz.yyyyy.zzzzzzz.invoker");
        codegen.setBooleanGetterPrefix("is");
        codegen.setUseNullForUnknownEnumValue(true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX), "is");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.USE_NULL_FOR_UNKNOWN_ENUM_VALUE), Boolean.TRUE);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.model.oooooo");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.api.oooooo");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.invoker.oooooo");
        codegen.additionalProperties().put(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX, "getBoolean");
        codegen.additionalProperties().put(AbstractJavaCodegen.USE_NULL_FOR_UNKNOWN_ENUM_VALUE, "true");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.model.oooooo");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.model.oooooo");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.api.oooooo");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.api.oooooo");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.invoker.oooooo");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.invoker.oooooo");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX), "getBoolean");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.USE_NULL_FOR_UNKNOWN_ENUM_VALUE), Boolean.TRUE);
    }

    @Test
    public void toEnumValue(){
        final AbstractJavaCodegen codegen = new P_AbstractJavaCodegen();
        Assert.assertEquals(codegen.toEnumValue("1", "Integer"), "1");
        Assert.assertEquals(codegen.toEnumValue("42", "Double"), "42");
        Assert.assertEquals(codegen.toEnumValue("1337", "Long"), "1337l");
        Assert.assertEquals(codegen.toEnumValue("3.14", "Float"), "3.14f");
    }

    private static class P_AbstractJavaCodegen extends AbstractJavaCodegen {
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
