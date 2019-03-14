package org.openapitools.codegen.typescript.fetch;

import org.junit.Assert;
import org.junit.Test;
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;


public class TypeScriptFetchClientCodegenTest {
    @Test
    public void testSnapshotVersion() {
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT.[0-9]{12}$"));

        codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1-SNAPSHOT.[0-9]{12}$"));

    }

    @Test
    public void testWithoutSnapshotVersion() {
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT$"));

        codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1$"));

    }

}
