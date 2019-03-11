package org.openapitools.codegen.typescript.typescriptangular;

import io.swagger.v3.oas.models.OpenAPI;
import org.junit.Assert;
import org.junit.Test;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;

public class TypescriptAngularApiVersionTest {

    @Test
    public void testWithApiVersion() {
        final TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();

        codegen.additionalProperties().put("npmName", "just-a-test");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.setOpenAPI(api);

        codegen.processOpts();

        Assert.assertEquals(codegen.getNpmVersion(), "1.0.7");
    }

    @Test
    public void testWithoutNpmName() {
        final TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();

        OpenAPI api = TestUtils.createOpenAPI();

        codegen.setOpenAPI(api);

        codegen.processOpts();

        Assert.assertEquals(codegen.getNpmVersion(), "1.0.0");
    }

    @Test
    public void testWithNpmVersion() {
        final TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();

        codegen.additionalProperties().put("npmName", "just-a-test");
        codegen.additionalProperties().put("npmVersion", "2.0.0");

        OpenAPI api = TestUtils.createOpenAPI();
        codegen.setOpenAPI(api);

        codegen.processOpts();

        Assert.assertEquals(codegen.getNpmVersion(), "2.0.0");
    }


}
