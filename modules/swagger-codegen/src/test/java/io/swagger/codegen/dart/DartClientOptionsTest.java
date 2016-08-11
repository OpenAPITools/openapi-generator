package io.swagger.codegen.dart;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.DartClientCodegen;
import io.swagger.codegen.options.DartClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class DartClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private DartClientCodegen clientCodegen;

    public DartClientOptionsTest() {
        super(new DartClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(DartClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setBrowserClient(Boolean.valueOf(DartClientOptionsProvider.BROWSER_CLIENT_VALUE));
            times = 1;
            clientCodegen.setPubName(DartClientOptionsProvider.PUB_NAME_VALUE);
            times = 1;
            clientCodegen.setPubVersion(DartClientOptionsProvider.PUB_VERSION_VALUE);
            times = 1;
            clientCodegen.setPubDescription(DartClientOptionsProvider.PUB_DESCRIPTION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(DartClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
        }};
    }
}

