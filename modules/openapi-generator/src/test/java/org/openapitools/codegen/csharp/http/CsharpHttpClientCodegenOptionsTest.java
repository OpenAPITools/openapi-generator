package org.openapitools.codegen.csharp.http;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.CsharpHttpClientCodegen;
import org.openapitools.codegen.options.CsharpHttpClientCodegenOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class CsharpHttpClientCodegenOptionsTest extends AbstractOptionsTest {

    @Tested
    private CsharpHttpClientCodegen codegen;

    public CsharpHttpClientCodegenOptionsTest() {
        super(new CsharpHttpClientCodegenOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return codegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        // TODO: Complete options
        new Expectations(codegen) {{

        }};
    }
}

