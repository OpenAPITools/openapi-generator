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
        Map<String, String> propeties = new HashMap<>();
        propeties.put("npmName", "node-es6-test");
        propeties.put("npmVersion", "1.0.3");
        propeties.put("snapshot", "false");
        propeties.put("supportsES6", "false");

        return propeties;
    }

    @Override
    protected IntegrationTestPathsConfig getIntegrationTestPathsConfig() {
        return new IntegrationTestPathsConfig("typescript/node-es5");
    }
}
