package io.swagger.codegen.scala;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.AbstractIntegrationTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.ScalaClientCodegen;
import io.swagger.codegen.testutils.IntegrationTestPathsConfig;
import org.testng.annotations.Test;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class ScalaClientRequiredAttributesIntegrationTest extends AbstractIntegrationTest {

    public ScalaClientRequiredAttributesIntegrationTest() {
        generateSwaggerMetadata = false;

        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        systemPropertyOverrides = builder
                .put(CodegenConstants.APIS, Boolean.TRUE.toString())
                .put(CodegenConstants.MODELS, Boolean.TRUE.toString())
                .put(CodegenConstants.API_DOCS, Boolean.FALSE.toString())
                .put(CodegenConstants.MODEL_DOCS, Boolean.FALSE.toString())
                .put(CodegenConstants.API_TESTS, Boolean.FALSE.toString())
                .put(CodegenConstants.MODEL_TESTS, Boolean.FALSE.toString())
                .put(CodegenConstants.SUPPORTING_FILES, Boolean.FALSE.toString())
                .build();
    }

    @Override
    protected IntegrationTestPathsConfig getIntegrationTestPathsConfig() {
        return new IntegrationTestPathsConfig("scala/client/required-attributes");
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return new ScalaClientCodegen();
    }

    @Override
    protected Map<String, String> configProperties() {
        Map<String, String> properties = new HashMap<>();
        properties.put(CodegenConstants.EXCLUDE_TESTS, Boolean.TRUE.toString());
        return properties;
    }

    // TODO: Remove this when super.generatesCorrectDirectoryStructure() is re-enabled.
    @Test(description = "Verify Scala client's understanding of 'required' attributes. (disabled awaiting CI fix for integration tests classpath)", enabled = false)
    public void test() throws IOException {
        this.generatesCorrectDirectoryStructure();
    }
}
