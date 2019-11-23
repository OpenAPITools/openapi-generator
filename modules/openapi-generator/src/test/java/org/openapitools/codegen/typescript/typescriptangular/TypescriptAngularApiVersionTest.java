package org.openapitools.codegen.typescript.typescriptangular;

import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class TypescriptAngularApiVersionTest {

    @Test(description = "tests if API version specification is used if no version is provided in additional properties")
    public void testWithApiVersion() {
        final TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();

        codegen.additionalProperties().put("npmName", "just-a-test");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getNpmVersion(), "1.0.7");
    }

    @Test(description = "tests if npmVersion additional property is used")
    public void testWithNpmVersion() {
        final TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();

        codegen.additionalProperties().put("npmName", "just-a-test");
        codegen.additionalProperties().put("npmVersion", "2.0.0");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getNpmVersion(), "2.0.0");
    }

    @Test(description = "tests if default version is used when neither OpenAPI version nor npmVersion additional property has been provided")
    public void testWithoutApiVersion() {
        final TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();

        codegen.additionalProperties().put("npmName", "just-a-test");

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion(null);
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getNpmVersion(), "1.0.0");
    }

}
