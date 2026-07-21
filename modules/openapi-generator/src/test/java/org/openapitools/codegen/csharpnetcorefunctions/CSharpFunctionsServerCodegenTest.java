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

package org.openapitools.codegen.csharpnetcorefunctions;

import org.openapitools.codegen.languages.CSharpFunctionsServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class CSharpFunctionsServerCodegenTest {

    @Test
    public void testToEnumVarName() throws Exception {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.toEnumVarName("FooBar", "string"), "FooBarEnum");
        Assert.assertEquals(codegen.toEnumVarName("fooBar", "string"), "FooBarEnum");
        Assert.assertEquals(codegen.toEnumVarName("foo-bar", "string"), "FooBarEnum");
        Assert.assertEquals(codegen.toEnumVarName("foo_bar", "string"), "FooBarEnum");
        Assert.assertEquals(codegen.toEnumVarName("foo bar", "string"), "FooBarEnum");

        // The below cases do not work currently, camelize doesn't support uppercase
        // Assert.assertEquals(codegen.toEnumVarName("FOO-BAR", "string"), "FooBar");
        // Assert.assertEquals(codegen.toEnumVarName("FOO_BAR", "string"), "FooBar");
    }

    @Test
    public void abstractClassUsesDefaultVirtualOperations() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("classModifier", "abstract");

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("classModifier"), "abstract");
        Assert.assertEquals(codegen.additionalProperties().get("operationModifier"), "virtual");
        Assert.assertEquals(codegen.additionalProperties().get("generateBody"), Boolean.TRUE);
    }

    @Test
    public void abstractOperationsDisableBodyGeneration() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("classModifier", "abstract");
        codegen.additionalProperties().put("operationModifier", "abstract");

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("operationModifier"), "abstract");
        Assert.assertEquals(codegen.additionalProperties().get("generateBody"), Boolean.FALSE);
    }

    @Test
    public void libraryBuildUsesDefaultVirtualOperations() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("buildTarget", "library");

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("operationModifier"), "virtual");
        Assert.assertEquals(codegen.additionalProperties().get("generateBody"), Boolean.TRUE);
    }

    @Test
    public void isolatedWorkerSelectsIsolatedLibraryAndNet10() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("azureFunctionsVersion", "v4-isolated");

        codegen.processOpts();

        // the isolated template overrides are selected via the "isolated" library
        Assert.assertEquals(codegen.getLibrary(), "isolated");
        Assert.assertEquals(codegen.additionalProperties().get("isolatedWorker"), Boolean.TRUE);
        // the isolated worker only supports the net10.0 LTS target for now
        Assert.assertEquals(codegen.additionalProperties().get("targetFramework"), "net10.0");
        Assert.assertEquals(codegen.additionalProperties().get("netCoreVersion"), "10.0");
        // the "-isolated" suffix is stripped so the csproj emits <AzureFunctionsVersion>v4</...>
        Assert.assertEquals(codegen.additionalProperties().get("azureFunctionsVersion"), "v4");
        // System.Text.Json is used on the isolated path, not Newtonsoft
        Assert.assertEquals(codegen.additionalProperties().get("useNewtonsoft"), Boolean.FALSE);
        // a Program.cs host entry point is generated for the isolated worker
        Assert.assertTrue(codegen.supportingFiles().stream()
                .anyMatch(f -> "Program.cs".equals(f.getDestinationFilename())));
    }

    @Test
    public void isolatedWorkerOverridesUnsupportedNetVersion() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("azureFunctionsVersion", "v4-isolated");
        codegen.additionalProperties().put("netCoreVersion", "6.0");

        codegen.processOpts();

        // an unsupported netCoreVersion is forced back to net10.0
        Assert.assertEquals(codegen.additionalProperties().get("targetFramework"), "net10.0");
        Assert.assertEquals(codegen.additionalProperties().get("netCoreVersion"), "10.0");
    }

    @Test
    public void isolatedWorkerDefaultsToSystemTextJson() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("azureFunctionsVersion", "v4-isolated");

        codegen.processOpts();

        // with no explicit choice, the isolated worker serializes with System.Text.Json
        Assert.assertEquals(codegen.additionalProperties().get("useNewtonsoft"), Boolean.FALSE);
    }

    @Test
    public void isolatedWorkerHonorsExplicitNewtonsoft() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("azureFunctionsVersion", "v4-isolated");
        codegen.additionalProperties().put("useNewtonsoft", true);

        codegen.processOpts();

        // an explicit useNewtonsoft=true is honored on the isolated path
        Assert.assertEquals(codegen.additionalProperties().get("useNewtonsoft"), Boolean.TRUE);
        // it still selects the isolated library and net10.0 target
        Assert.assertEquals(codegen.getLibrary(), "isolated");
        Assert.assertEquals(codegen.additionalProperties().get("targetFramework"), "net10.0");
    }

    @Test
    public void isolatedWorkerDefaultsToAspNetCoreIntegration() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("azureFunctionsVersion", "v4-isolated");

        codegen.processOpts();

        // the isolated worker uses ASP.NET Core integration (HttpRequest/IActionResult) by default
        Assert.assertEquals(codegen.additionalProperties().get("aspNetCoreIntegration"), Boolean.TRUE);
    }

    @Test
    public void isolatedWorkerBuiltInModelWhenOptedOut() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("azureFunctionsVersion", "v4-isolated");
        codegen.additionalProperties().put("aspNetCoreIntegration", false);

        codegen.processOpts();

        // opting out selects the built-in model (HttpRequestData/HttpResponseData)
        Assert.assertEquals(codegen.additionalProperties().get("aspNetCoreIntegration"), Boolean.FALSE);
    }

    @Test
    public void isolatedWorkerOpenApiAttributesAreOptIn() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("azureFunctionsVersion", "v4-isolated");

        codegen.processOpts();

        // OpenAPI attributes are off by default
        Assert.assertEquals(codegen.additionalProperties().get("generateOpenApiAttributes"), Boolean.FALSE);
    }

    @Test
    public void isolatedWorkerEmitsOpenApiAttributesWhenEnabled() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();
        codegen.additionalProperties().put("azureFunctionsVersion", "v4-isolated");
        codegen.additionalProperties().put("generateOpenApiAttributes", true);

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get("generateOpenApiAttributes"), Boolean.TRUE);
    }

    @Test
    public void inProcessModelStaysOnDefaultLibrary() {
        final CSharpFunctionsServerCodegen codegen = new CSharpFunctionsServerCodegen();

        codegen.processOpts();

        // default (in-process) generation does not select the isolated library
        Assert.assertNull(codegen.getLibrary());
        Assert.assertFalse(codegen.supportingFiles().stream()
                .anyMatch(f -> "Program.cs".equals(f.getDestinationFilename())));
    }
}
