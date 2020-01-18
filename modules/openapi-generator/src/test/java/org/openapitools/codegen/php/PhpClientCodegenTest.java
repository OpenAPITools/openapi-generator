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

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.PhpClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class PhpClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final PhpClientCodegen codegen = new PhpClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "OpenAPI\\Client\\Model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "OpenAPI\\Client\\Model");
        Assert.assertEquals(codegen.apiPackage(), "OpenAPI\\Client\\Api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "OpenAPI\\Client\\Api");
        Assert.assertEquals(codegen.getInvokerPackage(), "OpenAPI\\Client");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PhpClientCodegen codegen = new PhpClientCodegen();
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
        final PhpClientCodegen codegen = new PhpClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "Xmodel");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "Xapi");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "Xinvoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "Xinvoker\\Xmodel");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "Xinvoker\\Xmodel");
        Assert.assertEquals(codegen.apiPackage(), "Xinvoker\\Xapi");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "Xinvoker\\Xapi");
        Assert.assertEquals(codegen.getInvokerPackage(), "Xinvoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "Xinvoker");
    }

}
