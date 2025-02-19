/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import org.openapitools.codegen.languages.PhpNextgenClientCodegen;
import org.openapitools.codegen.testutils.ConfigAssert;
import org.testng.Assert;
import org.testng.annotations.Test;

public class PhpNextgenClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.processOpts();

        Assert.assertEquals(codegen.isSupportStreaming(), false);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.setSupportStreaming(true);

        codegen.processOpts();

        Assert.assertEquals(codegen.isSupportStreaming(), true);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValuesWithFalseValue() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.additionalProperties().put(PhpNextgenClientCodegen.SUPPORT_STREAMING, false);

        codegen.processOpts();
        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());

        configAssert.assertValue(PhpNextgenClientCodegen.SUPPORT_STREAMING, codegen::isSupportStreaming, Boolean.FALSE);
        Assert.assertEquals(codegen.isSupportStreaming(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValuesWithTrueValue() throws Exception {
        final PhpNextgenClientCodegen codegen = new PhpNextgenClientCodegen();

        codegen.additionalProperties().put(PhpNextgenClientCodegen.SUPPORT_STREAMING, true);

        codegen.processOpts();
        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());

        configAssert.assertValue(PhpNextgenClientCodegen.SUPPORT_STREAMING, codegen::isSupportStreaming, Boolean.TRUE);
        Assert.assertEquals(codegen.isSupportStreaming(), true);
    }
}
