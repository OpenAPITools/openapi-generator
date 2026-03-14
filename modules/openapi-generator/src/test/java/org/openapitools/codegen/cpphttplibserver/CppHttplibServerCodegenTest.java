/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
package org.openapitools.codegen.cpphttplib;

import static org.openapitools.codegen.utils.StringUtils.underscore;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.CppHttplibServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class CppHttplibServerCodegenTest {

    CppHttplibServerCodegen codegen = new CppHttplibServerCodegen();

    @Test
    public void testInitialConfigValues() throws Exception {
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("projectName"), "cpp-httplib-server");
        Assert.assertEquals(codegen.modelPackage(), "cpp-httplib-server");
        Assert.assertEquals(codegen.additionalProperties().get("cmakeProjectName"), "cpp_httplib_server");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        codegen.additionalProperties().put("projectName", "TestProject");
        codegen.additionalProperties().put("modelNamespace", "test::models");
        codegen.additionalProperties().put("apiNamespace", "test::apis");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("projectName"), "TestProject");
        Assert.assertEquals(codegen.additionalProperties().get("modelNamespace"), "test::models");
        Assert.assertEquals(codegen.additionalProperties().get("apiNamespace"), "test::apis");
    }

    @Test
    public void testToModelName() {
        Assert.assertEquals(codegen.toModelName("User"), "User");
        Assert.assertEquals(codegen.toModelName("user"), "User");
        Assert.assertEquals(codegen.toModelName("user_model"), "UserModel");
        Assert.assertEquals(codegen.toModelName("object"), "InlineModel");
        Assert.assertEquals(codegen.toModelName("object_1"), "InlineModel1");
        Assert.assertEquals(codegen.toModelName("inline_object_1"), "InlineModel1");
        Assert.assertEquals(codegen.toModelName("_inline_model"), "Model");
    }

    @Test
    public void testToApiName() {
        Assert.assertEquals(codegen.toApiName("users"), "Users");
        Assert.assertEquals(codegen.toApiName("testproject"), "Testproject");
    }

    @Test
    public void testGetTypeDeclaration() {
        Assert.assertEquals(codegen.getTypeDeclaration(new io.swagger.v3.oas.models.media.StringSchema()), "std::string");
        Assert.assertEquals(codegen.getTypeDeclaration(new io.swagger.v3.oas.models.media.IntegerSchema()), "int");
        Assert.assertEquals(codegen.getTypeDeclaration(new io.swagger.v3.oas.models.media.BooleanSchema()), "bool");
        
        io.swagger.v3.oas.models.media.ArraySchema arraySchema = new io.swagger.v3.oas.models.media.ArraySchema();
        arraySchema.setItems(new io.swagger.v3.oas.models.media.StringSchema());
        Assert.assertEquals(codegen.getTypeDeclaration(arraySchema), "std::vector<std::string>");
    }

    @Test 
    public void testStringUtilityMethods() {
        Assert.assertEquals(codegen.toPascalCase("user_name"), "UserName");
        Assert.assertEquals(codegen.toPascalCase("userName"), "UserName");
        Assert.assertEquals(codegen.toCamelCase("user_name"), "userName");
        Assert.assertEquals(codegen.toPascalCase("UserName"), "UserName");
        Assert.assertEquals(codegen.toCamelCase("UserName"), "userName");
        Assert.assertEquals(underscore("UserName"), "user_name");
    }

    @Test
    public void testInlineObjectNaming() {
        // Test the enhanced inline object naming that we implemented
        Assert.assertEquals(codegen.toModelName("object"), "InlineModel");
        Assert.assertEquals(codegen.toModelName("object_1"), "InlineModel1");
        Assert.assertEquals(codegen.toModelName("object_123"), "InlineModel123");
        Assert.assertEquals(codegen.toModelName("inline_object"), "InlineModel");
        Assert.assertEquals(codegen.toModelName("inline_object_5"), "InlineModel5");
    }

    @Test
    public void testModelFilename() {
        Assert.assertEquals(codegen.toModelFilename("User"), "User");
        Assert.assertEquals(codegen.toModelFilename("UserModel"), "UserModel");
        Assert.assertEquals(codegen.toModelFilename("object"), "Object");
        Assert.assertEquals(codegen.toModelFilename("usermodel"), "Usermodel");
    }

    @Test
    public void testApiFilename() {
        Assert.assertEquals(codegen.toApiFilename("User"), "UserApi");
        Assert.assertEquals(codegen.toApiFilename("Default"), "DefaultApi");
    }

    @Test
    public void testTypeDeclarationWithAnyOf() {
        io.swagger.v3.oas.models.media.ComposedSchema anyOfSchema = new io.swagger.v3.oas.models.media.ComposedSchema();
        anyOfSchema.addAnyOfItem(new io.swagger.v3.oas.models.media.StringSchema());
        anyOfSchema.addAnyOfItem(new io.swagger.v3.oas.models.media.IntegerSchema());
        
        String result = codegen.getTypeDeclaration(anyOfSchema);
        Assert.assertTrue(result.contains("std::variant"), "anyOf should generate std::variant");
    }

    @Test
    public void testTypeDeclarationWithOneOf() {
        io.swagger.v3.oas.models.media.ComposedSchema oneOfSchema = new io.swagger.v3.oas.models.media.ComposedSchema();
        oneOfSchema.addOneOfItem(new io.swagger.v3.oas.models.media.StringSchema());
        oneOfSchema.addOneOfItem(new io.swagger.v3.oas.models.media.NumberSchema());
        
        String result = codegen.getTypeDeclaration(oneOfSchema);
        Assert.assertTrue(result.contains("std::variant"), "oneOf should generate std::variant");
    }

    @Test
    public void testModelNamespaceHandling() {
        codegen.additionalProperties().put("modelNamespace", "myapp::models");
        codegen.processOpts();
        
        Assert.assertEquals(codegen.additionalProperties().get("modelNamespace"), "myapp::models");
    }

    @Test
    public void testApiNamespaceHandling() {
        codegen.additionalProperties().put("apiNamespace", "myapp::api");
        codegen.processOpts();
        
        Assert.assertEquals(codegen.additionalProperties().get("apiNamespace"), "myapp::api");
    }
}
