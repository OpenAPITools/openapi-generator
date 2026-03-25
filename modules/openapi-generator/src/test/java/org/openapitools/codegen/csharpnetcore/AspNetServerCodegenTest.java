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

package org.openapitools.codegen.csharpnetcore;

import org.openapitools.codegen.languages.AspNetServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class AspNetServerCodegenTest {

    @Test
    public void abstractClassUsesDefaultVirtualOperations() {
        final AspNetServerCodegen codegen = new AspNetServerCodegen();
        codegen.additionalProperties().put("classModifier", "abstract");

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("classModifier"), "abstract");
        Assert.assertEquals(codegen.additionalProperties().get("operationModifier"), "virtual");
        Assert.assertEquals(codegen.additionalProperties().get("generateBody"), Boolean.TRUE);
    }

    @Test
    public void abstractOperationsDisableBodyGeneration() {
        final AspNetServerCodegen codegen = new AspNetServerCodegen();
        codegen.additionalProperties().put("classModifier", "abstract");
        codegen.additionalProperties().put("operationModifier", "abstract");

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("operationModifier"), "abstract");
        Assert.assertEquals(codegen.additionalProperties().get("generateBody"), Boolean.FALSE);
    }

    @Test
    public void libraryBuildUsesDefaultVirtualOperations() {
        final AspNetServerCodegen codegen = new AspNetServerCodegen();
        codegen.additionalProperties().put("buildTarget", "library");

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("classModifier"), "abstract");
        Assert.assertEquals(codegen.additionalProperties().get("operationModifier"), "virtual");
        Assert.assertEquals(codegen.additionalProperties().get("generateBody"), Boolean.TRUE);
    }
}
