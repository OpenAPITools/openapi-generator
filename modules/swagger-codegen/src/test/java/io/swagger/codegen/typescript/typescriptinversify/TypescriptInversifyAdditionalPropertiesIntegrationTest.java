package io.swagger.codegen.typescript.typescriptinversify;

import java.util.HashMap;
import java.util.Map;

import io.swagger.codegen.AbstractIntegrationTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TypeScriptInversifyClientCodegen;
import io.swagger.codegen.testutils.IntegrationTestPathsConfig;

public class TypescriptInversifyAdditionalPropertiesIntegrationTest extends AbstractIntegrationTest {

    @Override
    protected CodegenConfig getCodegenConfig() {
        return new TypeScriptInversifyClientCodegen();
    }

    @Override
    protected Map<String, String> configProperties() {
        Map<String, String> properties = new HashMap<>();
        properties.put("npmName", "additionalPropertiesTest");
        properties.put("npmVersion", "1.0.2");
        properties.put("snapshot", "false");

        return properties;
    }

    @Override
    protected IntegrationTestPathsConfig getIntegrationTestPathsConfig() {
        return new IntegrationTestPathsConfig("typescript/additional-properties");
    }
}
