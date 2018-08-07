package org.openapitools.codegen.kotlin-spring;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.KotlinSpringServerCodegen;
import org.openapitools.codegen.options.KotlinSpringServerCodegenOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class KotlinSpringServerCodegenOptionsTest extends AbstractOptionsTest {

    @Tested
    private KotlinSpringServerCodegen codegen;

    public KotlinSpringServerCodegenOptionsTest() {
        super(new KotlinSpringServerCodegenOptionsProvider());
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

