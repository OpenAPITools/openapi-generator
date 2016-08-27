package io.swagger.codegen.typescript.typescriptnode;

import java.util.HashMap;
import java.util.Map;

import io.swagger.codegen.AbstractIntegrationTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TypeScriptNodeClientCodegen;
import io.swagger.codegen.testutils.IntegrationTestPathsConfig;

public class TypescriptNodeES5IntegrationTest extends AbstractIntegrationTest {

    @Override
    protected CodegenConfig getCodegenConfig() {
        return new TypeScriptNodeClientCodegen();
    }

    @Override
    protected Map<String, String> configProperties() {
        Map<String, String> properties = new HashMap<>();
        properties.put("npmName", "node-es6-test");
        properties.put("npmVersion", "1.0.3");
        properties.put("snapshot", "false");
        properties.put("supportsES6", "false");

        return properties;
    }

    @Override
    protected IntegrationTestPathsConfig getIntegrationTestPathsConfig() {
        return new IntegrationTestPathsConfig("typescript/node-es5");
    }
}
