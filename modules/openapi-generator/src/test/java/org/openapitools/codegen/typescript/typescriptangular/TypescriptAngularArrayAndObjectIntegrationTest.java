package org.openapitools.codegen.typescript.typescriptangular;

import java.util.HashMap;
import java.util.Map;

import org.openapitools.codegen.AbstractIntegrationTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;
import org.openapitools.codegen.testutils.IntegrationTestPathsConfig;

public class TypescriptAngularArrayAndObjectIntegrationTest extends AbstractIntegrationTest {

    @Override
    protected CodegenConfig getCodegenConfig() {
        return new TypeScriptAngularClientCodegen();
    }

    @Override
    protected Map<String, String> configProperties() {
        Map<String, String> properties = new HashMap<>();
        properties.put("npmName", "arrayAndAnyTest");
        properties.put("npmVersion", "1.0.2");
        properties.put("snapshot", "false");

        return properties;
    }

    @Override
    protected IntegrationTestPathsConfig getIntegrationTestPathsConfig() {
        return new IntegrationTestPathsConfig("typescript/array-and-object");
    }
}
