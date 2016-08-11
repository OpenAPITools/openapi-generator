package io.swagger.codegen.typescript.typescriptangular2;

import java.util.HashMap;
import java.util.Map;

import io.swagger.codegen.AbstractIntegrationTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TypeScriptAngular2ClientCodegen;
import io.swagger.codegen.testutils.IntegrationTestPathsConfig;

public class TypescriptAngular2ArrayAndObjectTest extends AbstractIntegrationTest {

    @Override
    protected CodegenConfig getCodegenConfig() {
        return new TypeScriptAngular2ClientCodegen();
    }

    @Override
    protected Map<String, String> configProperties() {
        Map<String, String> propeties = new HashMap<>();
        propeties.put("npmName", "arrayAndAnyTest");
        propeties.put("npmVersion", "1.0.2");
        propeties.put("snapshot", "false");

        return propeties;
    }

    @Override
    protected IntegrationTestPathsConfig getIntegrationTestPathsConfig() {
        return new IntegrationTestPathsConfig("typescript/array-and-object");
    }
}
