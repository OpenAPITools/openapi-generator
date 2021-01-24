package org.openapitools.codegen.typescript.typescriptnestjs;

import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptNestjsClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class TypescriptNestjsApiVersionTest {

    @Test(description = "tests if API version specification is used if no version is provided in additional properties")
    public void testWithApiVersion() {
        final TypeScriptNestjsClientCodegen codegen = new TypeScriptNestjsClientCodegen();

        codegen.additionalProperties().put("npmName", "just-a-test");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getNpmVersion(), "1.0.7");
    }

    @Test(description = "tests if npmVersion additional property is used")
    public void testWithNpmVersion() {
        final TypeScriptNestjsClientCodegen codegen = new TypeScriptNestjsClientCodegen();

        codegen.additionalProperties().put("npmName", "just-a-test");
        codegen.additionalProperties().put("npmVersion", "2.0.0");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getNpmVersion(), "2.0.0");
    }

    @Test(description = "tests if default version is used when neither OpenAPI version nor npmVersion additional property has been provided")
    public void testWithoutApiVersion() {
        final TypeScriptNestjsClientCodegen codegen = new TypeScriptNestjsClientCodegen();

        codegen.additionalProperties().put("npmName", "just-a-test");

        OpenAPI api = TestUtils.createOpenAPI();
        api.getInfo().setVersion(null);
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertEquals(codegen.getNpmVersion(), "1.0.0");
    }

}
