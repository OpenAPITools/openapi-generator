package io.swagger.codegen.ruby;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.RubyClientCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.Map;

public class RubyClientOptionsTest extends AbstractOptionsTest {
    private static final String GEM_NAME_VALUE = "swagger_client_ruby";
    private static final String MODULE_NAME_VALUE = "SwaggerClientRuby";
    private static final String GEM_VERSION_VALUE = "1.0.0-SNAPSHOT";

    @Tested
    private RubyClientCodegen clientCodegen;

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setGemName(GEM_NAME_VALUE);
            times = 1;
            clientCodegen.setModuleName(MODULE_NAME_VALUE);
            times = 1;
            clientCodegen.setGemVersion(GEM_VERSION_VALUE);
            times = 1;
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(RubyClientCodegen.GEM_NAME, GEM_NAME_VALUE)
                .put(RubyClientCodegen.MODULE_NAME, MODULE_NAME_VALUE)
                .put(RubyClientCodegen.GEM_VERSION, GEM_VERSION_VALUE)
                .build();
    }
}
