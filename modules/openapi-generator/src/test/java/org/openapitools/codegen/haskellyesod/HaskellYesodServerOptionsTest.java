package org.openapitools.codegen.haskellyesod;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.HaskellYesodServerCodegen;
import org.openapitools.codegen.options.HaskellYesodServerOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class HaskellYesodServerOptionsTest extends AbstractOptionsTest {

    private HaskellYesodServerCodegen clientCodegen = mock(HaskellYesodServerCodegen.class, mockSettings);

    public HaskellYesodServerOptionsTest() {
        super(new HaskellYesodServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setProjectName(HaskellYesodServerOptionsProvider.PROJECT_NAME_VALUE);
        verify(clientCodegen).setApiModuleName(HaskellYesodServerOptionsProvider.API_MODULE_NAME_VALUE);
    }
}
