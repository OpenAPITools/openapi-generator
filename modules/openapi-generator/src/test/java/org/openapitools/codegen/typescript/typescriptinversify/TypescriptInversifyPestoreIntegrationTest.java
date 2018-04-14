package org.openapitools.codegen.typescript.typescriptinversify;

import java.util.HashMap;
import java.util.Map;

import org.openapitools.codegen.AbstractIntegrationTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptInversifyClientCodegen;
import org.openapitools.codegen.testutils.IntegrationTestPathsConfig;

public class TypescriptInversifyPestoreIntegrationTest extends AbstractIntegrationTest {

    @Override
    protected CodegenConfig getCodegenConfig() {
        return new TypeScriptInversifyClientCodegen();
    }

    @Override
    protected Map<String, String> configProperties() {
        Map<String, String> properties = new HashMap<>();
        properties.put("npmName", "petstore-integration-test");
        properties.put("npmVersion", "1.0.3");
        properties.put("snapshot", "false");
        properties.put("usePromise", "false");

        return properties;
    }

    @Override
    protected IntegrationTestPathsConfig getIntegrationTestPathsConfig() {
        return new IntegrationTestPathsConfig("typescript/petstore");
    }
}
