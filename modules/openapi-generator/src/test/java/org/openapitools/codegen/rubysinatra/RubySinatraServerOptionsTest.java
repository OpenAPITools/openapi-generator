package org.openapitools.codegen.rubysinatra;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.RubySinatraServerCodegen;
import org.openapitools.codegen.options.RubySinatraServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class RubySinatraServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private RubySinatraServerCodegen clientCodegen;

    public RubySinatraServerOptionsTest() {
        super(new RubySinatraServerOptionsProvider());
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
