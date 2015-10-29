package io.swagger.codegen.sinatra;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.SinatraServerCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.Map;

public class SinatraServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private SinatraServerCodegen clientCodegen;

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        //SinatraServerCodegen doesn't have its own options and base options are cleared
        return ImmutableMap.of();
    }
}
