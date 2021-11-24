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

package org.openapitools.codegen.java.jaxrs;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.languages.AbstractJavaJAXRSServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.*;

public class AbstractJavaJAXRSServerCodegenTest {

    private final AbstractJavaJAXRSServerCodegen fakeJavaJAXRSCodegen = new P_AbstractJavaJAXRSServerCodegen();

    @Test
    public void convertApiName() {
        Assert.assertEquals(fakeJavaJAXRSCodegen.toApiName("name"), "NameApi");
        Assert.assertEquals(fakeJavaJAXRSCodegen.toApiName("$name"), "NameApi");
        Assert.assertEquals(fakeJavaJAXRSCodegen.toApiName("nam#e"), "NameApi");
        Assert.assertEquals(fakeJavaJAXRSCodegen.toApiName("$another-fake?"), "AnotherFakeApi");
        Assert.assertEquals(fakeJavaJAXRSCodegen.toApiName("fake_classname_tags 123#$%^"), "FakeClassnameTags123Api");
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final AbstractJavaJAXRSServerCodegen codegen = new P_AbstractJavaJAXRSServerCodegen();
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaJAXRSServerCodegen.SERVER_PORT), "8082");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final AbstractJavaJAXRSServerCodegen codegen = new P_AbstractJavaJAXRSServerCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xx.yyyyyyyy.model");
        codegen.setApiPackage("xx.yyyyyyyy.api");
        codegen.setInvokerPackage("xx.yyyyyyyy.invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
        Assert.assertEquals(codegen.modelPackage(), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.apiPackage(), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xx.yyyyyyyy.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xx.yyyyyyyy.invoker");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final AbstractJavaJAXRSServerCodegen codegen = new P_AbstractJavaJAXRSServerCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.aaaaa.api");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.iiii.invoker");
        codegen.additionalProperties().put("serverPort", "8088");
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(AbstractJavaJAXRSServerCodegen.SERVER_PORT), "8088");
    }

    @Test
    public void testCommonPath() {
        final AbstractJavaJAXRSServerCodegen codegen = new P_AbstractJavaJAXRSServerCodegen();
        Map<String, Object> objs = new HashMap<>();
        Map<String, List<CodegenOperation>> opMap = new HashMap<>();
        List<CodegenOperation> operations = new ArrayList<>();
        objs.put("operations", opMap);
        opMap.put("operation", operations);

        operations.add(getCo("/"));
        codegen.postProcessOperationsWithModels(objs, Collections.emptyList());
        Assert.assertEquals(objs.get("commonPath"), "");
        Assert.assertEquals(operations.get(0).path, "");
        Assert.assertFalse(operations.get(0).subresourceOperation);
        operations.clear();

        operations.add(getCo("/test"));
        codegen.postProcessOperationsWithModels(objs, Collections.emptyList());
        Assert.assertEquals(objs.get("commonPath"), "/test");
        Assert.assertEquals(operations.get(0).path, "");
        Assert.assertFalse(operations.get(0).subresourceOperation);
        operations.clear();

        operations.add(getCo("/"));
        operations.add(getCo("/op1"));
        codegen.postProcessOperationsWithModels(objs, Collections.emptyList());
        Assert.assertEquals(objs.get("commonPath"), "");
        Assert.assertEquals(operations.get(0).path, "/");
        Assert.assertFalse(operations.get(0).subresourceOperation);
        Assert.assertEquals(operations.get(1).path, "/op1");
        Assert.assertTrue(operations.get(1).subresourceOperation);
        operations.clear();

        operations.add(getCo("/group1/subgroup1/op1"));
        operations.add(getCo("/group1/subgroup1/op2"));
        codegen.postProcessOperationsWithModels(objs, Collections.emptyList());
        Assert.assertEquals(objs.get("commonPath"), "/group1/subgroup1");
        Assert.assertEquals(operations.get(0).path, "/op1");
        Assert.assertTrue(operations.get(0).subresourceOperation);
        Assert.assertEquals(operations.get(1).path, "/op2");
        Assert.assertTrue(operations.get(1).subresourceOperation);
        operations.clear();

        operations.add(getCo("/op1"));
        operations.add(getCo("/op2"));
        codegen.postProcessOperationsWithModels(objs, Collections.emptyList());
        Assert.assertEquals(objs.get("commonPath"), "");
        Assert.assertEquals(operations.get(0).path, "/op1");
        Assert.assertTrue(operations.get(0).subresourceOperation);
        Assert.assertEquals(operations.get(1).path, "/op2");
        Assert.assertTrue(operations.get(1).subresourceOperation);
        operations.clear();

        operations.add(getCo("/group1"));
        operations.add(getCo("/group1/op1"));
        codegen.postProcessOperationsWithModels(objs, Collections.emptyList());
        Assert.assertEquals(objs.get("commonPath"), "/group1");
        Assert.assertEquals(operations.get(0).path, "");
        Assert.assertFalse(operations.get(0).subresourceOperation);
        Assert.assertEquals(operations.get(1).path, "/op1");
        Assert.assertTrue(operations.get(1).subresourceOperation);
        operations.clear();
    }

    private CodegenOperation getCo(final String path) {
        final CodegenOperation co = new CodegenOperation();
        co.path = path;
        return co;
    }

    private static class P_AbstractJavaJAXRSServerCodegen extends AbstractJavaJAXRSServerCodegen {

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
