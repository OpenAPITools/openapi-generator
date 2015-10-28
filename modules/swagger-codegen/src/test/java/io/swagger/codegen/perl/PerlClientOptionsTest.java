package io.swagger.codegen.perl;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.PerlClientCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.Map;

public class PerlClientOptionsTest extends AbstractOptionsTest {
    private static final String MODULE_NAME_VALUE = "";
    private static final String MODULE_VERSION_VALUE = "";

    @Tested
    private PerlClientCodegen clientCodegen;

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModuleName(MODULE_NAME_VALUE);
            times = 1;
            clientCodegen.setModuleVersion(MODULE_VERSION_VALUE);
            times = 1;
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(PerlClientCodegen.MODULE_NAME, MODULE_NAME_VALUE)
                .put(PerlClientCodegen.MODULE_VERSION, MODULE_VERSION_VALUE)
                .build();
    }
}
