/*
 * Copyright 2020 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import org.openapitools.codegen.languages.CSharpNetCoreClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class CSharpNetCoreClientCodegenTest {

    @Test
    public void testToEnumVarName() throws Exception {
        final CSharpNetCoreClientCodegen codegen = new CSharpNetCoreClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.toEnumVarName("FooBar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("fooBar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("foo-bar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("foo_bar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("foo bar", "string"), "FooBar");

        // The below cases do not work currently, camelize doesn't support uppercase
        // Assert.assertEquals(codegen.toEnumVarName("FOO-BAR", "string"), "FooBar");
        // Assert.assertEquals(codegen.toEnumVarName("FOO_BAR", "string"), "FooBar");
    }
}
