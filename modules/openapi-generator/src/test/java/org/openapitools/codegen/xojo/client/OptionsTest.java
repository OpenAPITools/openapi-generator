package org.openapitools.codegen.xojo.client;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.XojoClientCodegen;
import org.openapitools.codegen.options.OptionsProvider;
import org.openapitools.codegen.options.XojoClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class OptionsTest extends AbstractOptionsTest {
    private XojoClientCodegen clientCodegen = mock(XojoClientCodegen.class, mockSettings);

    public OptionsTest() {
        super(new XojoClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setProjectName(XojoClientOptionsProvider.PROJECT_NAME_VALUE);
        verify(clientCodegen).setNonPublicApi(Boolean.parseBoolean(XojoClientOptionsProvider.NON_PUBLIC_API_VALUE));
        verify(clientCodegen).setPrependFormOrBodyParameters(Boolean.valueOf(XojoClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
        verify(clientCodegen).setEnumUnknownDefaultCase(Boolean.parseBoolean(XojoClientOptionsProvider.ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
        verify(clientCodegen).setSerializationLibrary(XojoClientOptionsProvider.SERIALIZATION_LIBRARY_VALUE);
        verify(clientCodegen).setLibrary(XojoClientOptionsProvider.LIBRARY_VALUE);
    }
}

