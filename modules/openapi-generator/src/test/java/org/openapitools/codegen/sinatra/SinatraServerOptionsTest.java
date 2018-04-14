package org.openapitools.codegen.sinatra;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.SinatraServerCodegen;
import org.openapitools.codegen.options.SinatraServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class SinatraServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private SinatraServerCodegen clientCodegen;

    public SinatraServerOptionsTest() {
        super(new SinatraServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
        }};
    }
}
